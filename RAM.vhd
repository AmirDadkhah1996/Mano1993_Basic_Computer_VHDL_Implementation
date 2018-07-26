library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity RAM is
	port (
    clk, wr_en : in std_logic;
		address: in std_logic_vector (9 downto 0);
		data_in : in std_logic_vector (15 downto 0);
    data_out: out std_logic_vector (15 downto 0)
  );
end entity RAM;

architecture RAM_Arch of RAM is
	type Memory is array (0 to 1023) of std_logic_vector (15 downto 0);
  signal data_table : Memory := (others => (others => '0'));

begin
	process (clk)
		variable init : boolean := true;

	begin
		if init = true then
			-- some initiation
			data_table(0) <= "0001010000011010";
			data_table(1) <= "0011110000001000";
			data_table(2) <= "0011110000010000";
			data_table(3) <= "0011110000100000";
			data_table(4) <= "0011110000000001";
			data_table(10) <= "0001010000011100";
      data_table(11) <= "0100000000000001";
      data_table(12) <= "0011110000000001";
      data_table(13) <= "0011110000000010";
      data_table(14) <= "0011110000000100";
      data_table(15) <= "0011110000001000";
      data_table(16) <= "0011110000010000";
      data_table(17) <= "0011110000100000";
      -- data_table(18) <= "0011110001000000";
      data_table(19) <= "1011110000010000";
      -- data_table(19) <= "1011110000000001";
      -- data_table(20) <= "1011110000000010";
      -- data_table(21) <= "1011110000000100";
      -- data_table(22) <= "1011110000001000";
      -- data_table(24) <= "1011110000100000";
      -- data_table(25) <= "1011110001000000";
      -- data_table(26) <= "1011110010000000";
			init := false;
		end if;

		if  clk'event and clk = '1' then
			if wr_en = '1' then -- Writing :)
			   data_table(to_integer(unsigned(address))) <= data_in;
      end if;
		end if;
	end process;

  data_out <= data_table(to_integer(unsigned(address)));

end architecture RAM_Arch;
