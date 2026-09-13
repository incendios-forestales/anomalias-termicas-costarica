<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.34" styleCategories="Symbology">
  <pipe>
    <provider>
      <resampling enabled="false" zoomedInResamplingMethod="nearestNeighbour" zoomedOutResamplingMethod="nearestNeighbour" maxOversampling="2"/>
    </provider>
    <rasterrenderer type="singlebandpseudocolor" band="3" opacity="1" alphaBand="-1" nodataColor="" classificationMin="0" classificationMax="180">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>None</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <rastershader>
        <colorrampshader colorRampType="INTERPOLATED" classificationMode="1" clip="0" minimumValue="0" maximumValue="180" labelPrecision="0">
      <item alpha="255" value="0" color="#FCFFA4" label="0"/>
      <item alpha="255" value="30" color="#FCB519" label="30"/>
      <item alpha="255" value="60" color="#ED6925" label="60"/>
      <item alpha="255" value="90" color="#BB3754" label="90"/>
      <item alpha="255" value="120" color="#781C6D" label="120"/>
      <item alpha="255" value="150" color="#330A5F" label="150"/>
      <item alpha="255" value="180" color="#000004" label="≥ 180"/>
          <rampLegendSettings minimumLabel="" maximumLabel="" prefix="" suffix="" direction="0" orientation="2" useContinuousLegend="1"><numericFormat id="basic"><Option type="Map"><Option name="decimals" type="int" value="0"/></Option></numericFormat></rampLegendSettings>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast brightness="0" contrast="0" gamma="1"/>
    <huesaturation saturation="0" grayscaleMode="0" colorizeOn="0" colorizeRed="255" colorizeGreen="128" colorizeBlue="128" colorizeStrength="100" invertColors="0"/>
    <rasterresampler maxOversampling="2"/>
    <resamplingStage>resamplingFilter</resamplingStage>
  </pipe>
  <blendMode>0</blendMode>
  <!-- Longitud de la temporada (días): banda 3 del GeoTIFF consolidado; ver README -->
</qgis>
