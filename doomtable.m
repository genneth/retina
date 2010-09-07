function doomtable

doomxls = xlsread('bigTableOfDoom.xls', '', 'Sheet 1', 'basic');

cs = doomxls(17:22,3:17);
tot = sum(sum(cs(1:(end-1),:)));
ps = doomxls(29:34,3:17);

chances = arrayfun(@(s,p) binochance(s,tot,p), cs, ps);

dlmwrite('bigTableOfDoomChances.txt', chances, '\t');

fh = fopen('bigTableOfDoomData.tex', 'w');

[rows, cols] = size(cs);
fprintf(fh, '~ & pp & pr & pa & pb & pm & rr & ra & rb & rm & aa & ab & am & bb & bm & mm \\\\\n');
fprintf(fh, '\\hline\n');
rowh = {'p', 'r', 'a', 'b', 'm'};
for i = 1:(rows-1)
    fprintf(fh, rowh{i});
    for j = 1:cols
        if chances(i,j) < 1/15787.192684
            fprintf(fh, ' & \\multicolumn{1}{>{\\color{white}\\columncolor{foursd}}c}{${%d\\,}^{%.1f}_{%.0f}$}', cs(i,j), ps(i,j)*tot, 1/chances(i,j));
        elseif chances(i,j) < 1/370.398347380
            fprintf(fh, ' & \\multicolumn{1}{>{\\color{white}\\columncolor{threesd}}c}{${%d\\,}^{%.1f}_{%.0f}$}', cs(i,j), ps(i,j)*tot, 1/chances(i,j));
        else
            fprintf(fh, ' & {${%d\\,}^{%.1f}_{%.0f}$}', cs(i,j), ps(i,j)*tot, 1/chances(i,j));
        end
    end
    fprintf(fh, ' \\\\\n');
end

fclose(fh);

end