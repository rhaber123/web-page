<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <xsl:apply-templates select="/source/*"/>
</xsl:template>

<xsl:template match="h1">
     <xsl:copy use-attribute-sets="H1">
          <xsl:value-of select="."/>
     </xsl:copy>
</xsl:template>

<xsl:template match="p">
     <xsl:copy use-attribute-sets="P ">
          <xsl:value-of select="."/>
     </xsl:copy>
</xsl:template>

<xsl:attribute-set name="H1">
     <xsl:attribute name="align">center</xsl:attribute>
     <xsl:attribute name="style">color:red</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="P">
     <xsl:attribute name="align">left</xsl:attribute>
     <xsl:attribute name="style">color:blue</xsl:attribute>
</xsl:attribute-set>

</xsl:stylesheet>