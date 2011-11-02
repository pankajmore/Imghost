jQuery(function($){
    var slider = window.slider = new Slider($('#sliderContainer'));
    slider.setSize(800, 500);
    slider.fetchJson('image/json/10');
    slider.setTransitionFunction(SliderTransitionFunctions.circles);
});
