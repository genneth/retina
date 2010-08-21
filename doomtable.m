function doomtable

doomxls = xlsread('bigTableOfDoom.xls', '', 'Sheet 1', 'basic');

cs = doomxls(16:21,3:17)
tot = sum(sum(cs(1:(end-1),:)))
ps = doomxls(28:33,3:17)

chances = arrayfun(@(s,p) binochance(s,tot,p), cs, ps)

dlmwrite('bigTableOfDoomChances.txt', chances, '\t');

end