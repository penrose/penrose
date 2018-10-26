var config = require("../config/webpack.config.dev");
module.exports = (storybookBaseConfig, configType) => {
    storybookBaseConfig.module.rules = storybookBaseConfig.module.rules.concat(
        config.module.rules
    );
    storybookBaseConfig.plugins = storybookBaseConfig.plugins.concat(
        config.plugins
    );
    storybookBaseConfig.resolve.extensions = storybookBaseConfig.resolve.extensions.concat(
        config.resolve.extensions
    );
    return storybookBaseConfig;
};
