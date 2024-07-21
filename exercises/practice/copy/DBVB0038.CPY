      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS VM
      *            Datum      :  25.02.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBVB0038.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/cpy/DBVB0038.CPv  $
      *
      *   Rev 2.2   Feb 26 2003 10:07:48   t13ex22
      *FIS00006: Weitere Feldlängen erweitert
      *
      *   Rev 2.1   Feb 25 2003 15:28:20   t13ex22
      *FIS00006: Neues Map-Copy wegen geänderter Map
      *
      *   Rev 2.0   Feb 25 2003 14:46:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   Micro Focus BMS Screen Painter (ver MFBMW 2.5.10)
      *   MapSet Name   BVB0038
      *   Date Created  02/26/2003
      *   Time Created  09:16:57

      *  Input Data For Map BVB0038
         01 BVB0038I.
            03 FILLER                         PIC X(12).
            03 ADIRNRL                        PIC S9(4) COMP.
            03 ADIRNRF                        PIC X.
            03 FILLER REDEFINES ADIRNRF.
               05 ADIRNRA                        PIC X.
            03 ADIRNRI                        PIC X(3).
            03 ESTNRL                         PIC S9(4) COMP.
            03 ESTNRF                         PIC X.
            03 FILLER REDEFINES ESTNRF.
               05 ESTNRA                         PIC X.
            03 ESTNRI                         PIC X(7).
            03 ASTPRZL                        PIC S9(4) COMP.
            03 ASTPRZF                        PIC X.
            03 FILLER REDEFINES ASTPRZF.
               05 ASTPRZA                        PIC X.
            03 ASTPRZI                        PIC X(1).
            03 AMSBEZL                        PIC S9(4) COMP.
            03 AMSBEZF                        PIC X.
            03 FILLER REDEFINES AMSBEZF.
               05 AMSBEZA                        PIC X.
            03 AMSBEZI                        PIC X(4).
            03 AVBPBEZL                       PIC S9(4) COMP.
            03 AVBPBEZF                       PIC X.
            03 FILLER REDEFINES AVBPBEZF.
               05 AVBPBEZA                       PIC X.
            03 AVBPBEZI                       PIC X(2).
            03 ELETMONL                       PIC S9(4) COMP.
            03 ELETMONF                       PIC X.
            03 FILLER REDEFINES ELETMONF.
               05 ELETMONA                       PIC X.
            03 ELETMONI                       PIC X(3).
            03 EMONVONL                       PIC S9(4) COMP.
            03 EMONVONF                       PIC X.
            03 FILLER REDEFINES EMONVONF.
               05 EMONVONA                       PIC X.
            03 EMONVONI                       PIC X(4).
            03 EMONBISL                       PIC S9(4) COMP.
            03 EMONBISF                       PIC X.
            03 FILLER REDEFINES EMONBISF.
               05 EMONBISA                       PIC X.
            03 EMONBISI                       PIC X(4).
            03 AVBNAMEL                       PIC S9(4) COMP.
            03 AVBNAMEF                       PIC X.
            03 FILLER REDEFINES AVBNAMEF.
               05 AVBNAMEA                       PIC X.
            03 AVBNAMEI                       PIC X(31).
            03 ASTICHL                        PIC S9(4) COMP.
            03 ASTICHF                        PIC X.
            03 FILLER REDEFINES ASTICHF.
               05 ASTICHA                        PIC X.
            03 ASTICHI                        PIC X(10).
            03 AKTOTXTL                       PIC S9(4) COMP.
            03 AKTOTXTF                       PIC X.
            03 FILLER REDEFINES AKTOTXTF.
               05 AKTOTXTA                       PIC X.
            03 AKTOTXTI                       PIC X(14).
            03 APROVGL                        PIC S9(4) COMP.
            03 APROVGF                        PIC X.
            03 FILLER REDEFINES APROVGF.
               05 APROVGA                        PIC X.
            03 APROVGI                        PIC X(11).
            03 APROVGWL                       PIC S9(4) COMP.
            03 APROVGWF                       PIC X.
            03 FILLER REDEFINES APROVGWF.
               05 APROVGWA                       PIC X.
            03 APROVGWI                       PIC X(3).
            03 AKTODISL                       PIC S9(4) COMP.
            03 AKTODISF                       PIC X.
            03 FILLER REDEFINES AKTODISF.
               05 AKTODISA                       PIC X.
            03 AKTODISI                       PIC X(10).
            03 AKTODIWL                       PIC S9(4) COMP.
            03 AKTODIWF                       PIC X.
            03 FILLER REDEFINES AKTODIWF.
               05 AKTODIWA                       PIC X.
            03 AKTODIWI                       PIC X(3).
            03 AKTODIHL                       PIC S9(4) COMP.
            03 AKTODIHF                       PIC X.
            03 FILLER REDEFINES AKTODIHF.
               05 AKTODIHA                       PIC X.
            03 AKTODIHI                       PIC X(1).
            03 APROVEL                        PIC S9(4) COMP.
            03 APROVEF                        PIC X.
            03 FILLER REDEFINES APROVEF.
               05 APROVEA                        PIC X.
            03 APROVEI                        PIC X(11).
            03 APROVEWL                       PIC S9(4) COMP.
            03 APROVEWF                       PIC X.
            03 FILLER REDEFINES APROVEWF.
               05 APROVEWA                       PIC X.
            03 APROVEWI                       PIC X(3).
            03 AKTODARL                       PIC S9(4) COMP.
            03 AKTODARF                       PIC X.
            03 FILLER REDEFINES AKTODARF.
               05 AKTODARA                       PIC X.
            03 AKTODARI                       PIC X(10).
            03 AKTODAWL                       PIC S9(4) COMP.
            03 AKTODAWF                       PIC X.
            03 FILLER REDEFINES AKTODAWF.
               05 AKTODAWA                       PIC X.
            03 AKTODAWI                       PIC X(3).
            03 AKTODAHL                       PIC S9(4) COMP.
            03 AKTODAHF                       PIC X.
            03 FILLER REDEFINES AKTODAHF.
               05 AKTODAHA                       PIC X.
            03 AKTODAHI                       PIC X(1).
            03 APROVBL                        PIC S9(4) COMP.
            03 APROVBF                        PIC X.
            03 FILLER REDEFINES APROVBF.
               05 APROVBA                        PIC X.
            03 APROVBI                        PIC X(11).
            03 APROVBWL                       PIC S9(4) COMP.
            03 APROVBWF                       PIC X.
            03 FILLER REDEFINES APROVBWF.
               05 APROVBWA                       PIC X.
            03 APROVBWI                       PIC X(3).
            03 AKTORUEL                       PIC S9(4) COMP.
            03 AKTORUEF                       PIC X.
            03 FILLER REDEFINES AKTORUEF.
               05 AKTORUEA                       PIC X.
            03 AKTORUEI                       PIC X(10).
            03 AKTORUWL                       PIC S9(4) COMP.
            03 AKTORUWF                       PIC X.
            03 FILLER REDEFINES AKTORUWF.
               05 AKTORUWA                       PIC X.
            03 AKTORUWI                       PIC X(3).
            03 AKTORUHL                       PIC S9(4) COMP.
            03 AKTORUHF                       PIC X.
            03 FILLER REDEFINES AKTORUHF.
               05 AKTORUHA                       PIC X.
            03 AKTORUHI                       PIC X(1).
            03 ARUEAUSL                       PIC S9(4) COMP.
            03 ARUEAUSF                       PIC X.
            03 FILLER REDEFINES ARUEAUSF.
               05 ARUEAUSA                       PIC X.
            03 ARUEAUSI                       PIC X(11).
            03 ARUEAUWL                       PIC S9(4) COMP.
            03 ARUEAUWF                       PIC X.
            03 FILLER REDEFINES ARUEAUWF.
               05 ARUEAUWA                       PIC X.
            03 ARUEAUWI                       PIC X(3).
            03 ARUEZUFL                       PIC S9(4) COMP.
            03 ARUEZUFF                       PIC X.
            03 FILLER REDEFINES ARUEZUFF.
               05 ARUEZUFA                       PIC X.
            03 ARUEZUFI                       PIC X(10).
            03 ARUEZUWL                       PIC S9(4) COMP.
            03 ARUEZUWF                       PIC X.
            03 FILLER REDEFINES ARUEZUWF.
               05 ARUEZUWA                       PIC X.
            03 ARUEZUWI                       PIC X(3).
            03 ARUEZUHL                       PIC S9(4) COMP.
            03 ARUEZUHF                       PIC X.
            03 FILLER REDEFINES ARUEZUHF.
               05 ARUEZUHA                       PIC X.
            03 ARUEZUHI                       PIC X(1).
            03 AERFPRL                        PIC S9(4) COMP.
            03 AERFPRF                        PIC X.
            03 FILLER REDEFINES AERFPRF.
               05 AERFPRA                        PIC X.
            03 AERFPRI                        PIC X(11).
            03 AERFPRWL                       PIC S9(4) COMP.
            03 AERFPRWF                       PIC X.
            03 FILLER REDEFINES AERFPRWF.
               05 AERFPRWA                       PIC X.
            03 AERFPRWI                       PIC X(3).
            03 AAUFBOL                        PIC S9(4) COMP.
            03 AAUFBOF                        PIC X.
            03 FILLER REDEFINES AAUFBOF.
               05 AAUFBOA                        PIC X.
            03 AAUFBOI                        PIC X(11).
            03 AAUFBOWL                       PIC S9(4) COMP.
            03 AAUFBOWF                       PIC X.
            03 FILLER REDEFINES AAUFBOWF.
               05 AAUFBOWA                       PIC X.
            03 AAUFBOWI                       PIC X(3).
            03 AEDPRL                         PIC S9(4) COMP.
            03 AEDPRF                         PIC X.
            03 FILLER REDEFINES AEDPRF.
               05 AEDPRA                         PIC X.
            03 AEDPRI                         PIC X(11).
            03 AEDPRWL                        PIC S9(4) COMP.
            03 AEDPRWF                        PIC X.
            03 FILLER REDEFINES AEDPRWF.
               05 AEDPRWA                        PIC X.
            03 AEDPRWI                        PIC X(3).
            03 APFANZL                        PIC S9(4) COMP.
            03 APFANZF                        PIC X.
            03 FILLER REDEFINES APFANZF.
               05 APFANZA                        PIC X.
            03 APFANZI                        PIC X(3).
            03 AOLZL                          PIC S9(4) COMP.
            03 AOLZF                          PIC X.
            03 FILLER REDEFINES AOLZF.
               05 AOLZA                          PIC X.
            03 AOLZI                          PIC X(11).
            03 AOLZWL                         PIC S9(4) COMP.
            03 AOLZWF                         PIC X.
            03 FILLER REDEFINES AOLZWF.
               05 AOLZWA                         PIC X.
            03 AOLZWI                         PIC X(3).
            03 APFUNTTL                       PIC S9(4) COMP.
            03 APFUNTTF                       PIC X.
            03 FILLER REDEFINES APFUNTTF.
               05 APFUNTTA                       PIC X.
            03 APFUNTTI                       PIC X(1).
            03 APFUNTL                        PIC S9(4) COMP.
            03 APFUNTF                        PIC X.
            03 FILLER REDEFINES APFUNTF.
               05 APFUNTA                        PIC X.
            03 APFUNTI                        PIC X(11).
            03 APFUNTWL                       PIC S9(4) COMP.
            03 APFUNTWF                       PIC X.
            03 FILLER REDEFINES APFUNTWF.
               05 APFUNTWA                       PIC X.
            03 APFUNTWI                       PIC X(3).
            03 ASONSTL                        PIC S9(4) COMP.
            03 ASONSTF                        PIC X.
            03 FILLER REDEFINES ASONSTF.
               05 ASONSTA                        PIC X.
            03 ASONSTI                        PIC X(11).
            03 ASONSTWL                       PIC S9(4) COMP.
            03 ASONSTWF                       PIC X.
            03 FILLER REDEFINES ASONSTWF.
               05 ASONSTWA                       PIC X.
            03 ASONSTWI                       PIC X(3).
            03 APFSONTL                       PIC S9(4) COMP.
            03 APFSONTF                       PIC X.
            03 FILLER REDEFINES APFSONTF.
               05 APFSONTA                       PIC X.
            03 APFSONTI                       PIC X(1).
            03 AUNKASL                        PIC S9(4) COMP.
            03 AUNKASF                        PIC X.
            03 FILLER REDEFINES AUNKASF.
               05 AUNKASA                        PIC X.
            03 AUNKASI                        PIC X(11).
            03 AUNKASWL                       PIC S9(4) COMP.
            03 AUNKASWF                       PIC X.
            03 FILLER REDEFINES AUNKASWF.
               05 AUNKASWA                       PIC X.
            03 AUNKASWI                       PIC X(3).
            03 APFEINL                        PIC S9(4) COMP.
            03 APFEINF                        PIC X.
            03 FILLER REDEFINES APFEINF.
               05 APFEINA                        PIC X.
            03 APFEINI                        PIC X(11).
            03 APFEINWL                       PIC S9(4) COMP.
            03 APFEINWF                       PIC X.
            03 FILLER REDEFINES APFEINWF.
               05 APFEINWA                       PIC X.
            03 APFEINWI                       PIC X(3).
            03 ASUMAUSL                       PIC S9(4) COMP.
            03 ASUMAUSF                       PIC X.
            03 FILLER REDEFINES ASUMAUSF.
               05 ASUMAUSA                       PIC X.
            03 ASUMAUSI                       PIC X(11).
            03 ASUMAUWL                       PIC S9(4) COMP.
            03 ASUMAUWF                       PIC X.
            03 FILLER REDEFINES ASUMAUWF.
               05 ASUMAUWA                       PIC X.
            03 ASUMAUWI                       PIC X(3).
            03 ENUMMERL                       PIC S9(4) COMP.
            03 ENUMMERF                       PIC X.
            03 FILLER REDEFINES ENUMMERF.
               05 ENUMMERA                       PIC X.
            03 ENUMMERI                       PIC X(2).

      *  Output Data For Map BVB0038
         01 BVB0038O REDEFINES BVB0038I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 ADIRNRO                        PIC X(3).
            03 FILLER                         PIC X(3).
            03 ESTNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 ASTPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AMSBEZO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 AVBPBEZO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 ELETMONO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 EMONVONO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 EMONBISO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 AVBNAMEO                       PIC X(31).
            03 FILLER                         PIC X(3).
            03 ASTICHO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 AKTOTXTO                       PIC X(14).
            03 FILLER                         PIC X(3).
            03 APROVGO                        PIC X(11).
            03 FILLER                         PIC X(3).
            03 APROVGWO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 AKTODISO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AKTODIWO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 AKTODIHO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 APROVEO                        PIC X(11).
            03 FILLER                         PIC X(3).
            03 APROVEWO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 AKTODARO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AKTODAWO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 AKTODAHO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 APROVBO                        PIC X(11).
            03 FILLER                         PIC X(3).
            03 APROVBWO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 AKTORUEO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AKTORUWO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 AKTORUHO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 ARUEAUSO                       PIC X(11).
            03 FILLER                         PIC X(3).
            03 ARUEAUWO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 ARUEZUFO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 ARUEZUWO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 ARUEZUHO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 AERFPRO                        PIC X(11).
            03 FILLER                         PIC X(3).
            03 AERFPRWO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 AAUFBOO                        PIC X(11).
            03 FILLER                         PIC X(3).
            03 AAUFBOWO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 AEDPRO                         PIC X(11).
            03 FILLER                         PIC X(3).
            03 AEDPRWO                        PIC X(3).
            03 FILLER                         PIC X(3).
            03 APFANZO                        PIC X(3).
            03 FILLER                         PIC X(3).
            03 AOLZO                          PIC X(11).
            03 FILLER                         PIC X(3).
            03 AOLZWO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 APFUNTTO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 APFUNTO                        PIC X(11).
            03 FILLER                         PIC X(3).
            03 APFUNTWO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 ASONSTO                        PIC X(11).
            03 FILLER                         PIC X(3).
            03 ASONSTWO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 APFSONTO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 AUNKASO                        PIC X(11).
            03 FILLER                         PIC X(3).
            03 AUNKASWO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 APFEINO                        PIC X(11).
            03 FILLER                         PIC X(3).
            03 APFEINWO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 ASUMAUSO                       PIC X(11).
            03 FILLER                         PIC X(3).
            03 ASUMAUWO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 ENUMMERO                       PIC X(2).


