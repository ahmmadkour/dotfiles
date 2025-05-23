local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>f', builtin.find_files, { desc = 'Telescope find files' })
vim.keymap.set('n', '<leader>F', builtin.git_files, { desc = "Telescope find git files" })
vim.keymap.set('n', '<leader>s', builtin.live_grep, { desc = 'Telescope live grep' })
vim.keymap.set('n', '<leader>S', builtin.grep_string, { desc = "Telescope grep string" })
vim.keymap.set('n', '<leader>b', builtin.buffers, { desc = 'Telescope buffers' })
vim.keymap.set('n', '<leader>th', builtin.help_tags, { desc = 'Telescope help tags' })
