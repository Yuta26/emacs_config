
((digest . "899bedf3c6087d3bae18db85c5b22257") (undo-list nil (1722 . 1726) nil (#("
" 0 1 (fontified t)) . -1721) ((marker . 4063) . -1) ((marker . 4063) . -1) nil (#("
" 0 1 (fontified t)) . -1722) ((marker . 1722) . -1) ((marker . 1722) . -1) nil (#("
" 0 1 (fontified t)) . -1723) ((marker . 4012) . -1) ((marker . 4012) . -1) nil (#("
" 0 1 (fontified t rear-nonsticky t)) . -2514) ((marker . 1842) . -1) ((marker . 1842) . -1) nil (#("
@if ($job->run_mode == 'production')
      {{--  $productionNum = $job->production_num; $totalNum = $job->total_num;  --}}
      {{--  <td>{{{ $productionNum }}}</td> --}}
      {{--  <td>{{{ $totalNum - $productionNum }}}</td> --}}
      {{--  <td>{{{ $totalNum }}}</td> --}}
      {{-- 作業中ユーザー件数はボタン押下で非同期に取得させる  --}}
      <td></td>
      <td></td>
      <td></td>
      <td class=\"working_worker_num_td text-center\">
        <button class='btn btn-default btn_each_working_worker_num'>
        {{ trans(\"app.action.display\") }}
        </button>
        <br/>
        <span class='working_worker_num_span'><span class='replace_area_span'></span></span>
      </td>
    @else
      <td></td>
      <td></td>
      <td></td>
      <td></td>
    @endif
" 0 1 (fontified t) 1 4 (fontified t face c-annotation-face) 4 6 (fontified t) 6 7 (fontified t face default) 7 10 (fontified t face font-lock-variable-name-face) 10 12 (fontified t) 12 20 (fontified t face font-lock-variable-name-face) 20 24 (fontified t) 24 36 (fontified t face font-lock-string-face) 36 38 (fontified t) 38 50 (fontified t) 50 51 (fontified t face default) 51 64 (fontified t face font-lock-variable-name-face) 64 67 (fontified t) 67 68 (fontified t face default) 68 71 (fontified t face font-lock-variable-name-face) 71 73 (fontified t) 73 87 (fontified t face font-lock-variable-name-face) 87 89 (fontified t) 89 90 (fontified t face default) 90 98 (fontified t face font-lock-variable-name-face) 98 101 (fontified t) 101 102 (fontified t face default) 102 105 (fontified t face font-lock-variable-name-face) 105 107 (fontified t) 107 116 (fontified t face font-lock-variable-name-face) 116 144 (fontified t) 144 145 (fontified t face default) 145 158 (fontified t face font-lock-variable-name-face) 158 193 (fontified t) 193 194 (fontified t face default) 194 202 (fontified t face font-lock-variable-name-face) 202 205 (fontified t) 205 206 (fontified t face default) 206 219 (fontified t face font-lock-variable-name-face) 219 234 (fontified t) 234 254 (fontified t) 254 255 (fontified t face default) 255 263 (fontified t face font-lock-variable-name-face) 263 379 (fontified t) 379 384 (fontified t face font-lock-keyword-face) 384 385 (fontified t) 385 420 (fontified t face font-lock-string-face) 420 438 (fontified t) 438 443 (fontified t face font-lock-keyword-face) 443 444 (fontified t) 444 489 (fontified t face font-lock-string-face) 489 508 (fontified t) 508 528 (fontified t face font-lock-string-face) 528 533 (fontified t) 533 551 (fontified t) 551 579 (fontified t) 579 584 (fontified t face font-lock-keyword-face) 584 585 (fontified t) 585 610 (fontified t face font-lock-string-face) 610 617 (fontified t) 617 622 (fontified t face font-lock-keyword-face) 622 623 (fontified t) 623 642 (fontified t face font-lock-string-face) 642 674 (fontified t) 674 679 (fontified t face c-annotation-face) 679 744 (fontified t) 744 748 (fontified t) 748 754 (fontified t face c-annotation-face) 754 755 (fontified t)) . -2516) ((marker . 2130) . -755) ((marker . 2130) . -755) ((marker . 2183) . -744) ((marker . 2183) . -744) ((marker . 2252) . -728) ((marker . 2252) . -728) ((marker . 2294) . -712) ((marker . 2294) . -712) ((marker . 2312) . -696) ((marker . 2312) . -696) ((marker . 2326) . -680) ((marker . 2326) . -680) ((marker . 2419) . -670) ((marker . 2419) . -670) ((marker . 2431) . -658) ((marker . 2431) . -658) ((marker . 2441) . -565) ((marker . 2441) . -565) ((marker . 2457) . -551) ((marker . 2457) . -551) ((marker . 2473) . -533) ((marker . 2473) . -533) ((marker . 2489) . -491) ((marker . 2489) . -491) ((marker . 2505) . -422) ((marker . 2505) . -422) ((marker . 2516) . -369) ((marker . 2516) . -369) ((marker . 2558) . -353) ((marker . 2558) . -353) ((marker . 2662) . -337) ((marker . 2662) . -337) ((marker . 2705) . -321) ((marker . 2705) . -321) ((marker . 2743) . -278) ((marker . 2743) . -278) ((marker . 2752) . -234) ((marker . 2752) . -234) ((marker . 2858) . -173) ((marker . 2858) . -173) ((marker . 2964) . -124) ((marker . 2964) . -124) ((marker . 3058) . -38) ((marker . 3058) . -38) ((marker . 3168) . -1) ((marker . 3168) . -1) ((marker) . -755) 3260 nil (nil rear-nonsticky nil 2514 . 2515) (nil fontified nil 2514 . 2515) (nil fontified nil 2508 . 2514) (nil fontified nil 2439 . 2508) (nil fontified nil 2434 . 2439) (nil fontified nil 2402 . 2434) (nil fontified nil 2383 . 2402) (nil fontified nil 2382 . 2383) (nil fontified nil 2377 . 2382) (nil fontified nil 2370 . 2377) (nil fontified nil 2345 . 2370) (nil fontified nil 2344 . 2345) (nil fontified nil 2339 . 2344) (nil fontified nil 2288 . 2339) (nil fontified nil 2268 . 2288) (nil fontified nil 2249 . 2268) (nil fontified nil 2204 . 2249) (nil fontified nil 2203 . 2204) (nil fontified nil 2198 . 2203) (nil fontified nil 2180 . 2198) (nil fontified nil 2145 . 2180) (nil fontified nil 2144 . 2145) (nil fontified nil 2139 . 2144) (nil fontified nil 2086 . 2139) (nil fontified nil 2076 . 2086) (nil fontified nil 2068 . 2076) (nil fontified nil 2067 . 2068) (nil fontified nil 2043 . 2067) (nil fontified nil 2030 . 2043) (nil fontified nil 2029 . 2030) (nil fontified nil 2026 . 2029) (nil fontified nil 2018 . 2026) (nil fontified nil 2017 . 2018) (nil fontified nil 1993 . 2017) (nil fontified nil 1980 . 1993) (nil fontified nil 1979 . 1980) (nil fontified nil 1966 . 1979) (nil fontified nil 1965 . 1966) (nil fontified nil 1964 . 1965) (nil fontified nil 1962 . 1964) (nil fontified nil 1960 . 1962) (nil fontified nil 1951 . 1960) (nil fontified nil 1949 . 1951) (nil fontified nil 1946 . 1949) (nil fontified nil 1945 . 1946) (nil fontified nil 1942 . 1945) (nil fontified nil 1934 . 1942) (nil fontified nil 1933 . 1934) (nil fontified nil 1931 . 1933) (nil fontified nil 1917 . 1931) (nil fontified nil 1915 . 1917) (nil fontified nil 1912 . 1915) (nil fontified nil 1911 . 1912) (nil fontified nil 1908 . 1911) (nil fontified nil 1895 . 1908) (nil fontified nil 1894 . 1895) (nil fontified nil 1893 . 1894) (nil fontified nil 1889 . 1893) (nil fontified nil 1882 . 1889) (nil fontified nil 1880 . 1882) (nil fontified nil 1868 . 1880) (nil fontified nil 1864 . 1868) (nil fontified nil 1856 . 1864) (nil fontified nil 1854 . 1856) (nil fontified nil 1851 . 1854) (nil fontified nil 1850 . 1851) (nil fontified nil 1848 . 1850) (nil fontified nil 1845 . 1848) (nil fontified nil 1841 . 1845) (1841 . 2515) nil (1842 . 1843) (#("    " 0 4 (fontified nil)) . 1842) (1841 . 1842) nil (1723 . 1725) (#("    " 0 4 (fontified nil)) . 1723) (1722 . 1723) (t 21904 59884 0 0) nil undo-tree-canary))
