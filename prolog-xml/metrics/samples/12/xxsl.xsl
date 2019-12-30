<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="dog">
     <p>
          <b>
               <xsl:text>Dog: </xsl:text>
          </b>
          <xsl:value-of select="@name"/>
     </p>
     <p>
          <b>
               <xsl:text>Color: </xsl:text>
          </b>
          <xsl:value-of select="data/@color"/>
     </p>
</xsl:template>


</xsl:stylesheet>