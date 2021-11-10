module Web.View.Static.Welcome where

import Web.View.Prelude

data WelcomeView = WelcomeView

instance View WelcomeView where
    html WelcomeView =
        [hsx|
            <canvas id="game-canvas" style="background-color: blue;"></canvas>
        |]