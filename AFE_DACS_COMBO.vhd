
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/27/2023 12:43:10 PM
-- Design Name: 
-- Module Name: SPI_DAC_DAPHNE - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- ----------------------------------------------------------------------------------
-- This firmware is made for the DAPHNE V3 to write to AD5327 in a daisy chaine configuration. 
-- if the AD5327 is not daisy chained, change the clk_togle_len to 16, same with the data_length. 
----------------------------------------------------------------------------------


LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;

ENTITY AFE_0_SPI IS
  GENERIC(
    clk_togle_len: integer :=24;
	dac_data_len: integer :=32;
    data_length : INTEGER := 24);     --data length in bits
  PORT(
    clk     : IN     STD_LOGIC;                             --system clock
    reset_n : IN     STD_LOGIC;                             --asynchronous active low reset
	reset_n1: in STD_LOGIC;                   -- for dacs trim and off
   -- enable  : IN     STD_LOGIC;                             --initiate communication   --- made this signals
	-- cpol    : IN     STD_LOGIC;  									--clock polarity mode      --- made this signals
  --  cpha    : IN     STD_LOGIC;                  --clock phase mode      --- made this signals
	chip_slector: in STD_LOGIC_VECTOR(1 downto 0);   
 
    miso    : IN     STD_LOGIC;                             --master in slave out
    sclk    : OUT    STD_LOGIC;                             --spi clock --- 10MHZ    worked better durring the test
    cs    : OUT    STD_LOGIC;                             --chip select
	CS_TRIM: OUT STD_LOGIC;
	CS_OFF: OUT STD_LOGIC;
    SDATA    : OUT    STD_LOGIC;                             --master out slave in
    --LDACN    : OUT    STD_LOGIC;                             -- USED FOR LDACN
	LDACN_TRIM: OUT STD_LOGIC;
	LDACN_OFF: OUT STD_LOGIC;
    tx		: IN     STD_LOGIC_VECTOR(data_length-1 DOWNTO 0);  --data to transmit
	TX_TRIM: IN STD_LOGIC_VECTOR (dac_data_len-1 DOWNTO 0);
	TX_OFF: IN STD_LOGIC_VECTOR(dac_data_len-1 DOWNTO 0);
    rx	   : OUT    STD_LOGIC_VECTOR(data_length-1 DOWNTO 0)); --data received
END AFE_0_SPI;

ARCHITECTURE behavioural OF AFE_0_SPI IS
  TYPE FSM IS(init, execute);                           		--state machine for afe
  SIGNAL state       : FSM;                             --state machine for offset and trim dacs
  TYPE FSM1 is (init1, execute1);
  SIGNAL state1 : FSM1; 
  SIGNAL receive_transmit : STD_LOGIC;                      --'1' for tx, '0' for rx 
  SIGNAL receive_transmit_trim_off: STD_LOGIC;   
  SIGNAL clk_toggles : INTEGER RANGE 0 TO clk_togle_len*2 + 1;    --clock toggle counter
  SIGNAL last_bit		: INTEGER RANGE 0 TO clk_togle_len*2;        --last bit indicator
  SIGNAL clk_toggles_trim_off : INTEGER RANGE 0 TO dac_data_len*2 + 1;    --clock toggle counter
  SIGNAL last_bit_trim_off		: INTEGER RANGE 0 TO dac_data_len*2;        --last bit indicator
  
  SIGNAL rxBuffer    : STD_LOGIC_VECTOR(data_length-1 DOWNTO 0) := (OTHERS => '0'); --receive data buffer
  SIGNAL txBuffer    : STD_LOGIC_VECTOR(data_length-1 DOWNTO 0) := (OTHERS => '0'); --transmit afe data buffer
  SIGNAL txBuffer_trim    : STD_LOGIC_VECTOR(dac_data_len-1 DOWNTO 0) := (OTHERS => '0'); --transmit trim dacs data buffer
  SIGNAL txBuffer_off   : STD_LOGIC_VECTOR(dac_data_len-1 DOWNTO 0) := (OTHERS => '0'); --transmit offeset dacs data buffer
  SIGNAL INT_ss_n    : STD_LOGIC;                            --Internal register for ss_n 
  SIGNAL INT_SS_N_TRIM: STD_LOGIC;
  SIGNAL INT_SS_OFF: STD_LOGIC;
  SIGNAL INT_afe_sclk    : STD_LOGIC;                            --Internal register for sclk 
  SIGNAL INT_off_sclk    : STD_LOGIC;                            --Internal register for sclk 
  SIGNAL INT_trim_sclk    : STD_LOGIC;                            --Internal register for sclk 
  SIGNAL enable    : STD_LOGIC;
  SIGNAL cpha    : STD_LOGIC;
  SIGNAL cpol    : STD_LOGIC;
  SIGNAL mosi_afe_buff    : STD_LOGIC;
  SIGNAL mosi_trim_buff    : STD_LOGIC;
  SIGNAL mosi_off_buff    : STD_LOGIC;
  SIGNAL sclk_off_buff    : STD_LOGIC;
  SIGNAL sclk_trim_buff    : STD_LOGIC;
  SIGNAL sclk_afe_buff    : STD_LOGIC;
  SIGNAL ldacn_trim_buff: STD_LOGIC;
  SIGNAL ldacn_off_buff: STD_LOGIC;
  SIGNAL CS_TRIM_BUFF : STD_LOGIC;
  SIGNAL CS_AFE_BUFF : STD_LOGIC;
  SIGNAL CS_OFF_BUFF: STD_LOGIC;

BEGIN
	
  
  enable <= '1';   
  cpha <= '1';
  cpol <= '1';
  CS_AFE_BUFF <= INT_ss_n;
  sclk_afe_buff <= INT_afe_sclk;
  CS_TRIM_BUFF <= INT_SS_N_TRIM;
  sclk_trim_buff <= INT_trim_sclk;
  CS_OFF_BUFF <= INT_SS_OFF;
  sclk_off_buff <= INT_off_sclk;
  

  
  
  
  -- process for afe data in and out. sending 8 bits and receiving 16.
  
  PROCESS(clk, reset_n)
  BEGIN

    IF(reset_n = '0') THEN        --reset everything
                
      INT_ss_n <= '1';   
      rx <= (OTHERS => '0');
	  mosi_afe_buff <= 'Z';
      state <= init;              

    ELSIF(falling_edge(clk)) THEN
      CASE state IS               

        WHEN init =>					 -- bus is idle
  
          INT_ss_n <= '1'; 		  
          mosi_afe_buff <= 'Z';             
   
          IF(enable = '1') THEN       		--initiate communication
 
                   
            INT_afe_sclk <= cpol;        		--set spi clock polarity
            receive_transmit <= NOT cpha; --set spi clock phase
            txBuffer <= tx;    				--put data to buffer to transmit
            clk_toggles <= 0;        		--initiate clock toggle counter
            last_bit <= data_length*2 + conv_integer(cpha) - 1; --set last rx data bit
            state <= execute;        
          ELSE
            state <= init;          
          END IF;


        WHEN execute =>

          INT_ss_n <= '0';           						--pull the slave select signal down
			 receive_transmit <= NOT receive_transmit;   --change receive transmit mode
          
			 -- counter
			 IF(clk_toggles = clk_togle_len*2 + 1) THEN
				clk_toggles <= 0;               				--reset counter
          ELSE
				clk_toggles <= clk_toggles + 1; 				--increment counter
          END IF;
            
          -- toggle sclk
          IF(clk_toggles <= clk_togle_len*2 AND INT_ss_n = '0') THEN 
            INT_afe_sclk <= NOT INT_afe_sclk; --toggle spi clock
          END IF;
            
          --receive 
          IF(receive_transmit = '0' AND clk_toggles < last_bit + 1 AND INT_ss_n = '0') THEN 
            rxBuffer <= rxBuffer(data_length-2 DOWNTO 0) & miso; 
          END IF;
            
          --transmit 
          IF(receive_transmit = '1' AND clk_toggles < last_bit) THEN 
            mosi_afe_buff <= txBuffer(data_length-1);                    
            txBuffer <= txBuffer(data_length-2 DOWNTO 0) & '0'; 
          END IF;
            
          --  resume the communication
          IF(clk_toggles = clk_togle_len*2 + 1) THEN   
            INT_ss_n <= '1';         
            mosi_afe_buff <= 'Z';             
            rx <= rxBuffer;    
            state <= init;          
          ELSE                       
            state <= execute;        
          END IF;
      END CASE;
    END IF;
  END PROCESS;   
  
  
  
  
  
  
  
  -- process for trim and offeset dacs data in and out. sending 32 bits not receiving any.
  
  PROCESS(clk, reset_n1)
  BEGIN

    IF(reset_n1 = '0') THEN        --reset everything
      ldacn_trim_buff <='1';
	  ldacn_off_buff <='1';                
      INT_SS_N_TRIM <= '1';   
	  INT_SS_OFF <= '1';
      mosi_trim_buff <= 'Z';   
	  mosi_off_buff <= 'Z';
      state1 <= init1;              

    ELSIF(falling_edge(clk)) THEN
      CASE state1 IS               

        WHEN init1 =>					 -- bus is idle
         ldacn_trim_buff <='0';
	     ldacn_off_buff <='0';                
         INT_SS_N_TRIM <= '1';   
	     INT_SS_OFF <= '1';
         mosi_trim_buff <= 'Z';   
	     mosi_off_buff <= 'Z';         
   
          IF(enable = '1') THEN       		--initiate communication
            ldacn_trim_buff <= '1';  
			ldacn_off_buff <= '1';
                     
            INT_trim_sclk <= cpol;        		--set spi clock polarity
			INT_off_sclk <= cpol;
            receive_transmit_trim_off <= NOT cpha; --set spi clock phase
            txBuffer_off <= TX_OFF;    				--put data to buffer to transmit
			txBuffer_trim <= TX_TRIM;
            clk_toggles_trim_off <= 0;        		--initiate clock toggle counter
            last_bit_trim_off <= dac_data_len*2 + conv_integer(cpha) - 1; --set last rx data bit
            state1 <= execute1;        
          ELSE
            state1 <= init1;          
          END IF;


        WHEN execute1 =>
          ldacn_off_buff <= '1'; 
		  ldacn_trim_buff <= '1';
          INT_SS_N_TRIM <= '0';           						--pull the slave select signal down
		  INT_SS_OFF <= '0';
		  receive_transmit_trim_off <= NOT receive_transmit_trim_off;   --change receive transmit mode
          
			 -- counter
			 IF(clk_toggles_trim_off = dac_data_len*2 + 1) THEN
				clk_toggles_trim_off <= 0;               				--reset counter
          ELSE
				clk_toggles_trim_off <= clk_toggles_trim_off + 1; 				--increment counter
          END IF;
            
          -- toggle sclk
          IF(clk_toggles_trim_off <= dac_data_len*2 AND (INT_SS_N_TRIM OR INT_SS_OFF) = '0') THEN 
            
			INT_off_sclk<= NOT INT_off_sclk; --toggle spi clock
			INT_trim_sclk<= NOT INT_trim_sclk; --toggle spi clock
          END IF;
            
            
          --transmit 
          IF(receive_transmit_trim_off = '1' AND clk_toggles_trim_off < last_bit_trim_off) THEN 
            mosi_trim_buff <= txBuffer_trim(dac_data_len-1); 
			mosi_off_buff <= txBuffer_off(dac_data_len-1);
            txBuffer_off <= txBuffer_off(dac_data_len-2 DOWNTO 0) & '0'; 
			txBuffer_trim <= txBuffer_trim(dac_data_len-2 DOWNTO 0) & '0';
          END IF;
            
          --  resume the communication
          IF(clk_toggles_trim_off = dac_data_len*2 + 1) THEN   
            ldacn_off_buff <= '0';
			ldacn_trim_buff <= '0';
            INT_SS_N_TRIM <= '1'; 
			INT_SS_OFF <= '1'; 
            mosi_trim_buff <= 'Z';             
            mosi_off_buff <= 'Z';  
            state1 <= init1;          
          ELSE                       
            state1 <= execute1;        
          END IF;
      END CASE;
    END IF;
  END PROCESS;   
   
  
    
  process(clk)
begin
    case (chip_slector)is
        when "01"=>  
  ---- process outputs
  
			-- desired output signals
			sclk <= sclk_trim_buff ;
			SDATA <= mosi_trim_buff ;
			LDACN_TRIM <= ldacn_trim_buff ;
			CS_TRIM <= CS_TRIM_BUFF  ;
			-- other signals
			
			LDACN_OFF <= '1'  ;
			CS_OFF <= '1'  ;
			
			cs <= '1' ;

		when "10"=> 
			-- desired output signals 
			sclk <= sclk_off_buff ;
			SDATA <= mosi_off_buff  ;
			LDACN_OFF <= ldacn_off_buff ;
			CS_OFF <= CS_OFF_BUFF ;
			-- other signals
			LDACN_TRIM <= '1' ;
			CS_TRIM <= '1'  ;
			
			cs <= '1' ;
		
		when "11"=> 	
			-- desired output signals
			cs <= CS_AFE_BUFF ;
			sclk <= sclk_afe_buff  ;
			SDATA <= mosi_afe_buff  ;
			LDACN_TRIM <= '1' ;
			CS_TRIM <= '1'  ;
			
			LDACN_OFF <= '1'  ;
			CS_OFF <= '1' ;
			-- other signals
        when others => 
            cs <= '1' ;
			sclk <= '1'  ;
			SDATA <= '1'  ;
			LDACN_TRIM <= '1' ;
			CS_TRIM <= '1'  ;
			
			LDACN_OFF <= '1'  ;
			CS_OFF <= '1' ;
    end case;
  end process;
  
  

  
END behavioural;

