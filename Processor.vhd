library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.std_logic_textio.all;
--use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity MicroProcessor is
	port
	(
		RESET 		:in std_logic;				-- RESET
		CLK 			:in std_logic;				-- clock
		ADDR 			:out std_logic_vector(9 downto 0);	-- address lines
		inport 		:in std_logic_vector(7 downto 0);		-- input port
		outport 	:out std_logic_vector(7 downto 0);	-- output port
		intr_in		:in std_logic;				-- interrupt for input
		intr_out	:in std_logic				-- interrupt for output
	);
end entity MicroProcessor;

architecture MicroProcessor_Arch of MicroProcessor is

					-- list of registers
	signal MBR 							:std_logic_vector(15 downto 0); -- Memory Buffer Register
	signal MAR 							:std_logic_vector(9 downto 0);	-- Memory Address Register
	signal AR 							:std_logic_vector(9 downto 0);	-- Address Register
	signal AC								:std_logic_vector(7 downto 0);	-- Accumulator
	signal CR								:std_logic_vector(7 downto 0);	-- Counter Register
	signal PC								:std_logic_vector(9 downto 0);	-- Program Counter
	signal OPR							:std_logic_vector(3 downto 0);	-- Operation Register
	signal INPR							:std_logic_vector(7 downto 0);	-- Input Register
	signal OUTR							:std_logic_vector(7 downto 0);	-- Output Register

					-- internals of MicroProcessor
	signal Q								:std_logic_vector(15 downto 0);	-- Instruction Decoder Output
	signal C								:std_logic_vector(4 downto 0);  -- Cycle Decoder Output
	signal SC								:std_logic_vector(1 downto 0);	-- Sequence Counter Output
	signal T								:std_logic_vector(3 downto 0);	-- Timing Signals
	signal S								:std_logic;			-- Start-Stop Flip-Flop
	signal EN_INST_DECODER	:std_logic;			-- enable signal for instruction decoder
	signal CLR_SC						:std_logic;			-- clear signal for sequence counter
	signal FGI							:std_logic;			-- input flag
	signal FGO							:std_logic;			-- output flag
	signal INF							:std_logic;			-- Interrupt Flag
	signal ZF 							:std_logic;			-- Zero Flag
	signal CF 							:std_logic;			-- Carry Flag
	signal R_TO_DEC 				:std_logic;			-- R Signal For Cycles Combination
	signal F_TO_DEC 				:std_logic;			-- F Signal For Cycles Combination
	signal G_TO_DEC 				:std_logic;			-- G Signal For Cycles Combination
	signal I0 							:std_logic;
	signal I1 							:std_logic;
	signal WR_EN_TO_MEM			:std_logic;
	signal ADDR_TO_MEM			:std_logic_vector(9 downto 0) := (others => 'Z');
	signal DATA_TO_MEM			:std_logic_vector(15 downto 0) := (others => 'Z');
	signal DATA_FROM_MEM		:std_logic_vector(15 downto 0) := (others => 'Z');

	component RAM is
		port (
	    clk : in std_logic;
			wr_en : in std_logic;
			address: in std_logic_vector (9 downto 0);
			data_in : in std_logic_vector (15 downto 0);
	    data_out: out std_logic_vector (15 downto 0)
	  );
	end component RAM;

	begin

		RandomAccessMemory: RAM port map(
			clk => CLK,
			wr_en => WR_EN_TO_MEM,
			address => ADDR_TO_MEM,
			data_in => DATA_TO_MEM,
			data_out => DATA_FROM_MEM
		);

		process(OPR)			-- instruction decoder
		begin
			-- if EN_INST_DECODER='1' then
				case OPR is
					when "0000" => Q <= "0000000000000001";
					when "0001" => Q <= "0000000000000010";
					when "0010" => Q <= "0000000000000100";
					when "0011" => Q <= "0000000000001000";
					when "0100" => Q <= "0000000000010000";
					when "0101" => Q <= "0000000000100000";
					when "0110" => Q <= "0000000001000000";
					when "0111" => Q <= "0000000010000000";
					when "1000" => Q <= "0000000100000000";
					when "1001" => Q <= "0000001000000000";
					when "1010" => Q <= "0000010000000000";
					when "1011" => Q <= "0000100000000000";
					when "1100" => Q <= "0001000000000000";
					when "1101" => Q <= "0010000000000000";
					when "1110" => Q <= "0100000000000000";
					when "1111" => Q <= "1000000000000000";
					when others => null;
				end case;
			-- end if;
		end process;

		process(CLK)		-- 2-bit sequence counter
		begin
			if S = '1' then
				if (CLK'event and CLK='1') then
						if CLR_SC = '1' then
							SC <= "00";
							-- CLR_SC <= '0'; --This is Wrong !!
						else
							SC <= SC + "01";
						end if;
		  		end if;
			else
				SC <= "ZZ";
			end if;
		end process;

		process(SC)			-- 2-to-4 decoder to generate timing signals T0, T1, T2, T3
		begin
				case SC is
						when "00" => T <= "0001";
						when "01" => T <= "0010";
						when "10" => T <= "0100";
						when "11" => T <= "1000";
						when others => null;
				end case;
			-- end if;
		end process;

		process(R_TO_DEC, F_TO_DEC, G_TO_DEC, CLK)
		begin
				if (G_TO_DEC = '0') and (F_TO_DEC = '0') and (R_TO_DEC = '0') then
					C <= "00001";
				elsif (G_TO_DEC = '0') and (F_TO_DEC = '0') and (R_TO_DEC = '1') then
					C <= "00010";
				elsif (G_TO_DEC = '0') and (F_TO_DEC = '1') and (R_TO_DEC = '0') then
					C <= "00100";
				elsif (G_TO_DEC = '0') and (F_TO_DEC = '1') and (R_TO_DEC = '1') then
					C <= "01000";
				elsif (G_TO_DEC = '1') and (F_TO_DEC = '0') and (R_TO_DEC = '0') then
					C <= "10000";
				else
					C <= "ZZZZZ";
				end if;
		end process;

		process(T, C, CLK)

		variable sum	:std_logic_vector(8 downto 0);
		variable operand1	:std_logic_vector(8 downto 0);
		variable operand2	:std_logic_vector(8 downto 0);


		begin
			if  CLK'event and CLK = '1' then
				if RESET = '1' then
					CLR_SC <= '1';
					S <= '1';
					INF <= '0';
					FGI <= '0';
					FGO <= '0';
					AC <= "00000000";
					PC <= "0000000000";
					R_TO_DEC <= '0';
					F_TO_DEC <= '0';
					G_TO_DEC <= '0';

				elsif (C(0) and T(0)) = '1' then -- Fetch & Decode
					CLR_SC <= '0';
					MAR <= PC;

					WR_EN_TO_MEM <= '0';
					ADDR_TO_MEM <= PC;

					report "F&D1";
				elsif (C(0) and T(1)) = '1' then -- Fetch & Decode
					-- MBR <= MEM(MAR) ;
					MBR <= DATA_FROM_MEM;

					PC <= PC + 1;

					report "F&D2";
				elsif (C(0) and T(2)) = '1' then -- Fetch & Decode
					I1 <= MBR(15);
					I0 <= MBR(14);
					OPR <= MBR(13 downto 10);
					AR <= MBR(9 downto 0);
					-- EN_INST_DECODER <= '1';

					report "F&D3";
				elsif ((not I0) and C(0) and T(3)) = '1' then -- Fetch & Decode :Go To Execute Cycle
					R_TO_DEC <= '1';
					F_TO_DEC <= '1';
					G_TO_DEC <= '0';

					CLR_SC <= '1'; --Check

					report "F&D4 Go To Execute Cycle";
				elsif ((not I1) and I0 and C(0) and T(3)) = '1' then -- Fetch & Decode :Go To Direct Cycle
					R_TO_DEC <= '0';
					F_TO_DEC <= '1';
					G_TO_DEC <= '0';

					CLR_SC <= '1'; --Check

					report "F&D4 Go To Direct Cycle";
				elsif (I1 and I0 and C(0) and T(3)) = '1' then -- Fetch & Decode :Go To InDirect Cycle
					R_TO_DEC <= '1';
					F_TO_DEC <= '0';
					G_TO_DEC <= '0';

					CLR_SC <= '1'; --Check

					report "F&D4 Go To InDirect Cycle";

				elsif (C(1) and T(0)) = '1' then -- InDirect Cycle
					CLR_SC <= '0'; --Check
					MAR <= AR;

					WR_EN_TO_MEM <= '0';
					ADDR_TO_MEM <= AR;

					report "InDir1";
				elsif (C(1) and T(1)) = '1' then -- InDirect Cycle
					-- MBR <= MEM(MAR);
					MBR <= DATA_FROM_MEM;

					report "InDir2";
				elsif (C(1) and T(2)) = '1' then -- InDirect Cycle
					AR <= MBR(9 downto 0);

					report "InDir3";
				elsif (C(1) and T(3)) = '1' then -- InDirect Cycle
					R_TO_DEC <= '0';
					F_TO_DEC <= '1';
					G_TO_DEC <= '0';

					report "InDir4";

				elsif (C(2) and T(0)) = '1' then -- Direct Cycle
					CLR_SC <= '0'; --Check
					MAR <= AR;

					WR_EN_TO_MEM <= '0';
					ADDR_TO_MEM <= AR;

					report "Dir1";
				elsif (C(2) and T(1)) = '1' then -- Direct Cycle
					-- MBR <= MEM(MAR);
					MBR <= DATA_FROM_MEM;

					report "Dir2";
				elsif (C(2) and T(2)) = '1' then -- Direct Cycle
					R_TO_DEC <= '1';
					F_TO_DEC <= '1';
					G_TO_DEC <= '0';
					CLR_SC <= '1';

					report "Dir3";

				elsif (Q(0) and C(3) and T(0)) = '1' then -- ADD (Add memory word to AC)
					-- AC <= AC + MBR(7 downto 0);
					operand1 := '0' & AC;
					operand2 := '0' & MBR(7 downto 0);
					sum := operand1 + operand2;
					AC <= sum(7 downto 0);
					CF <= sum(8);
					if sum = "000000000" then
						ZF <= '1';
					else
						ZF <= '0';
					end if;

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					CLR_SC <= '1';

					report "Exe ADD";

				elsif (Q(1) and C(3) and T(0)) = '1' then -- SUB (Subtract memory word from AC)
					-- AC <= AC - MBR(Data)
					operand1 := '0' & AC;
					operand2 := '0' & ((not MBR(7 downto 0)) + 1);
					sum := operand1 + operand2;
					AC <= sum(7 downto 0);
					CF <= sum(8);
					if sum = "000000000" then
						ZF <= '1';
					else
						ZF <= '0';
					end if;

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					CLR_SC <= '1';

					report "Exe SUB";

				elsif (Q(2) and C(3) and T(0)) = '1' then -- AND (AND memory word to AC)
					AC <= AC and MBR(7 downto 0);

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					CLR_SC <= '1';

					report "Exe AND";

				elsif (Q(3) and C(3) and T(0)) = '1' then -- OR (OR memory word to AC)
					AC <= AC or MBR(7 downto 0);

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					CLR_SC <= '1';

					report "Exe OR";

				elsif (Q(4) and C(3) and T(0)) = '1' then -- XOR (XOR memory word to AC)
					AC <= AC xor MBR(7 downto 0);

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					CLR_SC <= '1';

					report "Exe XOR";

				elsif (Q(5) and C(3) and T(0)) = '1' then -- LDA (Load memory word to AC)
					AC <= MBR(7 downto 0);

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					CLR_SC <= '1';

					report "Exe LDA";

				elsif (Q(6) and C(3) and T(0)) = '1' then -- STA (Store content to AC in memory)
					MAR <= AR;
					MBR(7 downto 0) <= AC;
					CLR_SC <= '0';	-- Check

					WR_EN_TO_MEM <= '1';
					ADDR_TO_MEM <= AR;

					report "Exe 1 STA";

				elsif (Q(6) and C(3) and T(1)) = '1' then -- STA (Store content to AC in memory)
					-- MEM(MAR) <= MBR(Data)
					-- WR_EN_TO_MEM <= '1';
					-- ADDR_TO_MEM <= MAR;
					DATA_TO_MEM <= ( "00000000" & MBR(7 downto 0) );

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					CLR_SC <= '1';	-- Check

					report "Exe 2 STA";

				elsif (Q(7) and C(3) and T(0)) = '1' then -- BUN (Branch unconditionally)
					PC <= AR;

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					CLR_SC <= '1';

					report "Exe BUN";

				elsif (Q(8) and C(3) and T(0)) = '1' then -- BSA (Branch and save return address)
					MBR(9 downto 0) <= PC;
					MAR <= AR;

					CLR_SC <= '0';	-- Check

					report "Exe 1 BSA";

				elsif (Q(8) and C(3) and T(1)) = '1' then -- BSA (Branch and save return address)
					-- MEM(AR) <= MBR(Address)
					WR_EN_TO_MEM <= '1';
					ADDR_TO_MEM <= AR;
					DATA_TO_MEM <= ( "000000" &  MBR(9 downto 0) );

					AR <= AR + 1;

					report "Exe 2 BSA";

				elsif (Q(8) and C(3) and T(2)) = '1' then -- BSA (Branch and save return address)
					PC <= AR;

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					CLR_SC <= '1';	-- Check

					report "Exe 3 BSA";

				elsif (Q(9) and C(3) and T(0)) = '1' then -- DSZ (Decrement and skip if zero)
					MBR(7 downto 0) <= MBR(7 downto 0) - 1;
					CLR_SC <= '0';	-- Check

					WR_EN_TO_MEM <= '1';
					ADDR_TO_MEM <= AR;

					report "Exe 1 DSZ";

				elsif (Q(9) and C(3) and T(1)) = '1' then -- DSZ (Decrement and skip if zero)
					-- MEM(MAR) <= MBR(Data)
					-- WR_EN_TO_MEM <= '1';
					-- ADDR_TO_MEM <= MAR;
					DATA_TO_MEM <= ( "00000000" & MBR(7 downto 0) );

					AC <= MBR(7 downto 0);

					report "Exe 2 DSZ";

				elsif (Q(9) and C(3) and T(2)) = '1' then -- DSZ (Decrement and skip if zero)
					if (AC = 0) then
						PC <= PC + 1;
					end if;

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					CLR_SC <= '1';	-- Check

					report "Exe 3 DSZ";

				elsif (Q(10) and C(3) and T(0)) = '1' then -- LDC (Load CR)
					CR <= MBR(7 downto 0);

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					CLR_SC <= '1';

					report "Exe LDC";

				elsif (Q(11) and C(3) and T(0)) = '1' then -- BZ (Branch if zero)
					if ZF = '1' then
						PC <= PC + 1;
					else
						PC <= MBR(9 downto 0);
					end if;

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					CLR_SC <= '1';

					report "Exe BZ";

				elsif (Q(12) and C(3) and T(0)) = '1' then -- BC (Branch if carry)
					if CF = '1' then
						PC <= PC + 1;
					else
						PC <= MBR(9 downto 0);
					end if;

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					CLR_SC <= '1';

					report "Exe BC";

				elsif (Q(13) and C(3) and T(0)) = '1' then -- NOP (No Operation)
					-- Do Nothing

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					CLR_SC <= '1';

					report "Exe NOP";

				elsif (Q(15) and (not I1) and (not I0) and MBR(0) and C(3) and T(0)) = '1' then -- CLA (Clear AC)
					AC <= "00000000";

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					report "Exe CLA";

				elsif (Q(15) and (not I1) and (not I0) and MBR(1) and C(3) and T(0)) = '1' then -- CLS (Clear all status flags)
					CF <= '0';
					ZF <= '0';

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					report "Exe CLS";

				elsif (Q(15) and (not I1) and (not I0) and MBR(2) and C(3) and T(0)) = '1' then -- CMA (Complement AC)
					AC <= not AC;

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					report "Exe CMA";

				elsif (Q(15) and (not I1) and (not I0) and MBR(3) and C(3) and T(0)) = '1' then -- SRA (Shift right AC)
					-- AC <= SHR(AC)
					-- AC(7) <= '0'
					AC <= std_logic_vector(unsigned(AC) srl 1);

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					report "Exe SRA";

				elsif (Q(15) and (not I1) and (not I0) and MBR(4) and C(3) and T(0)) = '1' then -- SLA (Shift left AC)
					-- AC <= SHL(AC)
					-- AC(0) <= '0'
					AC <= std_logic_vector(unsigned(AC) sll 1);

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					report "Exe SLA";

				elsif (Q(15) and (not I1) and (not I0) and MBR(5) and C(3) and T(0)) = '1' then -- INC (Increment AC)
					AC <= AC + 1;

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					report "Exe INC";

				elsif (Q(15) and (not I1) and (not I0) and MBR(6) and C(3) and T(0)) = '1' then -- HALT (Terminate program)
					S <= '0';

					report "Exe HALT";

				elsif (Q(15) and I1 and (not I0) and MBR(0) and C(3) and T(0)) = '1' then -- INP (Input character to AC)
					AC <= INPR;
					FGI <= '0';

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					report "Exe INP";

				elsif (Q(15) and I1 and (not I0) and MBR(1) and C(3) and T(0)) = '1' then -- OUT (Output character from AC)
					OUTR <= AC;
					FGO <= '0';

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					report "Exe OUT";

				elsif (Q(15) and I1 and (not I0) and MBR(2) and C(3) and T(0)) = '1' then -- SKI (Skip on input flag)
					if FGI = '1' then
						PC <= PC + 1;
					end if;

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					report "Exe SKI";

				elsif (Q(15) and I1 and (not I0) and MBR(3) and C(3) and T(0)) = '1' then -- SKO (Skip on output flag)
					if FGO = '1' then
						PC <= PC + 1;
					end if;

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					report "Exe SKO";

				elsif (Q(15) and I1 and (not I0) and MBR(4) and C(3) and T(0)) = '1' then -- ION (Interrupt on)
					INF <= '1';

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					report "Exe ION";

				elsif (Q(15) and I1 and (not I0) and MBR(5) and C(3) and T(0)) = '1' then -- IOF (Interrupt off)
					INF <= '0';

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					report "Exe IOF";

				elsif (Q(15) and I1 and (not I0) and MBR(6) and C(3) and T(0)) = '1' then -- SFI (Set input flag)
					FGI <= '1';

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					report "Exe SFI";

				elsif (Q(15) and I1 and (not I0) and MBR(7) and C(3) and T(0)) = '1' then -- SFO (Set output flag)
					FGO <= '1';

					if INF = '0' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '0';

					elsif INF = '1' then
						R_TO_DEC <= '0';
						F_TO_DEC <= '0';
						G_TO_DEC <= '1';
					end if;

					report "Exe SFO";

				elsif (C(4) and T(0)) = '1' then -- Interrupt Cycle
					MBR(9 downto 0) <= PC;
					INF <= '0';

					report "INTR1";

				elsif (C(4) and T(1)) = '1' then -- Interrupt Cycle
					PC <= "0000000000";
					MAR <= "0000000000";

					WR_EN_TO_MEM <= '1';
					ADDR_TO_MEM <= "0000000000";

					report "INTR2";

				elsif (C(4) and T(2)) = '1' then -- Interrupt Cycle
					-- MEM(MAR) <= MBR(Data)
					-- WR_EN_TO_MEM <= '1';
					-- ADDR_TO_MEM <= MAR;
					DATA_TO_MEM <= ( "00000000" &  MBR(7 downto 0) );

					PC <= PC + 1;

					report "INTR3";

				elsif (C(4) and T(3)) = '1' then -- Interrupt Cycle
					R_TO_DEC <= '0';
					F_TO_DEC <= '0';
					G_TO_DEC <= '0';

					report "INTR4";

				end if;
			end if;
		end process;

		INPR <= inport;
		outport <= OUTR;
		ADDR <= AR;
end architecture MicroProcessor_Arch;
