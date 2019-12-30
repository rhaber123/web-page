<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <xsl:apply-templates/>
</xsl:template>

<xsl:template match="/xslTutorial">
     <xsl:apply-templates/>
</xsl:template>

<xsl:template match="AAA">
     <div style="color:purple">
          <xsl:value-of select="name()"/>
          <xsl:text> id=</xsl:text>
          <xsl:value-of select="@id"/>
     </div>
     <xsl:apply-templates/>
</xsl:template>

<xsl:template match="BBB">
     <div style="color:blue">
          <xsl:value-of select="name()"/>
          <xsl:text> id=</xsl:text>
          <xsl:value-of select="@id"/>
     </div>
     <xsl:apply-templates/>
</xsl:template>

<xsl:template match="CCC">
     <div style="color:maroon">
          <xsl:value-of select="name()"/>
          <xsl:text> id=</xsl:text>
          <xsl:value-of select="@id"/>
     </div>
     <xsl:apply-templates/>
</xsl:template>

<xsl:template match="DDD">
     <div style="color:green">
          <xsl:value-of select="name()"/>
          <xsl:text> id=</xsl:text>
          <xsl:value-of select="@id"/>
     </div>
</xsl:template>


</xsl:stylesheet>