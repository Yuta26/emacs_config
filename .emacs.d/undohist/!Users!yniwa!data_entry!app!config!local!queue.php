
((digest . "683de5fe3b3a56a7b11598eb0949a5a4") (undo-list nil (nil rear-nonsticky nil 264 . 265) (nil fontified nil 1 . 265) (1 . 265) nil (#("<?php

return [
    'default' => isset($_ENV['queue.default']) ? $_ENV['queue.default'] : 'sync',
    'connections' => [
        'redis' => [
            'driver' => 'redis',
            'queue'  => 'data-entry-local'
        ]
    ],
    'failed' => [
        'database' => 'mysql',
        'table'    => 'failed_jobs'
    ],
];
" 0 1 (fontified t face font-lock-preprocessor-face category c-cpp-delimiter) 1 5 (fontified t face font-lock-preprocessor-face) 5 6 (fontified t category c-cpp-delimiter) 6 7 (fontified t) 7 13 (fontified t face font-lock-keyword-face) 13 20 (fontified t) 20 29 (fontified t face font-lock-string-face) 29 33 (fontified t) 33 38 (fontified t face font-lock-keyword-face) 38 39 (fontified t) 39 40 (fontified t face default) 40 44 (fontified t face font-lock-variable-name-face) 44 45 (fontified t) 45 60 (fontified t face font-lock-string-face) 60 65 (fontified t) 65 66 (fontified t face default) 66 70 (fontified t face font-lock-variable-name-face) 70 71 (fontified t) 71 86 (fontified t face font-lock-string-face) 86 90 (fontified t) 90 96 (fontified t face font-lock-string-face) 96 102 (fontified t) 102 115 (fontified t face font-lock-string-face) 115 129 (fontified t) 129 136 (fontified t face font-lock-string-face) 136 154 (fontified t) 154 162 (fontified t face font-lock-string-face) 162 166 (fontified t) 166 173 (fontified t face font-lock-string-face) 173 187 (fontified t) 187 194 (fontified t face font-lock-string-face) 194 199 (fontified t) 199 217 (fontified t face font-lock-string-face) 217 239 (fontified t) 239 247 (fontified t face font-lock-string-face) 247 261 (fontified t) 261 271 (fontified t face font-lock-string-face) 271 275 (fontified t) 275 282 (fontified t face font-lock-string-face) 282 292 (fontified t) 292 299 (fontified t face font-lock-string-face) 299 306 (fontified t) 306 319 (fontified t face font-lock-string-face) 319 330 (fontified t)) . 1) ((marker . 6) . -5) ((marker) . -6) ((marker) . -6) ((marker) . -7) ((marker) . -7) ((marker) . -16) ((marker) . -16) ((marker) . -98) ((marker) . -98) ((marker) . -121) ((marker) . -121) ((marker) . -142) ((marker) . -142) ((marker) . -175) ((marker) . -175) ((marker) . -218) ((marker) . -218) ((marker) . -228) ((marker) . -228) ((marker) . -235) ((marker) . -235) ((marker) . -253) ((marker) . -253) ((marker) . -284) ((marker) . -284) ((marker) . -320) ((marker) . -320) ((marker) . -327) ((marker) . -327) ((marker* . 265) . 330) ((marker* . 265) . 330) ((marker . 1) . -330) ((marker . 1) . -330) ((marker) . -330) 99 (t 21692 24357 0 0)))
