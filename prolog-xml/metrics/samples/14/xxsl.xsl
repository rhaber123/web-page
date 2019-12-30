<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="car[@checked]">
     <p>
          <xsl:text>Car: </xsl:text>
          <xsl:value-of select="@id"/>
     </p>
</xsl:template>


</xsl:stylesheet>