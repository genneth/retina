function doomtable

doomxls = xlsread('bigTableOfDoom.xls', '', 'Sheet 1', 'basic');

cs = doomxls(17:22,3:17);
tot = sum(sum(cs(1:(end-1),:)));
ps = doomxls(29:34,3:17);

chances = arrayfun(@(s,p) binochance(s,tot,p), cs, ps);

dlmwrite('bigTableOfDoomChances.txt', chances, '\t');

fh = fopen('bigTableOfDoomData.tex', 'w');

[rows, cols] = size(cs);
fprintf(fh, '~ & $2\\times$RPC & RPC RPh & RPC Am & RPC Bi & RPC Mu & $2\\times$RPh & RPh Am & RPh Bi & RPh Mu & $2\\times$Am & Am Bi & Am Mu & $2\\times$Bi & Bi Mu & $2\\times$Mu \\\\\n');
fprintf(fh, '\\hline\n');
rowh = {'RPC', 'RPh', 'Am', 'Bi', 'Mu'};
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
