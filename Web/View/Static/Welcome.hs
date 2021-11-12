module Web.View.Static.Welcome where

import Web.View.Prelude

data WelcomeView = WelcomeView

instance View WelcomeView where
    html WelcomeView =
        [hsx|
            <div class="d-flex flex-column">
                <canvas id="game-canvas" class="align-items-center"></canvas>
            </div>

            <style>
                #game-canvas {
                    background-color: white;
                    border-color: lightgrey;
                    border-width: 5px;
                    border-style: solid;
                    border-radius: 10px;
                }
            </style>
        |]