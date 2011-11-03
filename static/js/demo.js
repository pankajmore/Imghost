jQuery(function($){
    var slider = window.slider = new Slider($('#sliderContainer'));
    slider.setSize(800, 500);
    slider.fetchJson('json?count=15');
    slider.setTransitionFunction(SliderTransitionFunctions.circles);
});
