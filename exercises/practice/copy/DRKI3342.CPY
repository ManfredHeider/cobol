      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  03.05.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3342.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3342.CPv  $
      *
      *   Rev 2.2   Jun 07 2018 13:12:30   t13ae52
      *AKI01583: Programm: Ermittlung von Beitr./Prod./Kennz. für G
      *enerali-Gesellschaften
      *
      *   Rev 2.1   May 18 2018 11:17:32   t13ae52
      *AKI01583: Programm: Ermittlung von Beitr./Prod./Kennz. für G
      *enerali-Gesellschaften
      *
      *   Rev 2.0   May 03 2018 14:30:30   t13ae52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01583!03.05.18!    ! Neuerstellung             !SCHUMACHER*
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * COPYSTRECKE FÜR LIST-OZP ZXKI0493                            *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRKI3342.
           05  PI-DRKI3342-PROGRAMM            PIC  X(008).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DRKI3342-UFKT                 PIC  9(002).
              88 PI-DRKI3342-UFKT-FIRST                 VALUE 1.
              88 PI-DRKI3342-UFKT-NEXT                  VALUE 2.
              88 PI-DRKI3342-UFKT-QUIT                  VALUE 3.
           05 PI-DRKI3342-KEY.
      *       %M_PI_KEY%
              10 PI-DRKI3342-GVDVERTRAGSID   PIC  X(0013).
 
 
      ***
       01 PO-DRKI3342.
           05 PO-DRKI3342-RC                    PIC  X(003).
             88 PO-DRKI3342-RC-OK                        VALUE 'OK '.
             88 PO-DRKI3342-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI3342-RC-EOF                       VALUE 'EOF'.
 
           05  PO-DRKI3342-NI.
             10 NI-DRKI3342-TAB.
                15 DRKI3342-ELE
                   PIC S9(04) COMP OCCURS 20 TIMES.
             10 NI-DRKI3342 REDEFINES NI-DRKI3342-TAB.
ANF   *        %M_PO_DATEN_NI%
                15 PO-DRKI3342-NI-GVDABLAUF   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDAKTVWERT   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDAMTKZ   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDBEGINN   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDBEITRAG  PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDBERECHDAT   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDFLAECHE   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDGESCHAEFTSZW   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDGUTHABEN   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDLVART   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDNJBEITRAG   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDPRODKOMBI   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDPRODUKT   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDRENTEGARANT   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDRENTENBEZUG   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDRENTEVORAUS   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDSUMME   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDTARIF   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDTARPROD-KNR   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDTARPROD-KTY   PIC S9(4) COMP.
                15 PO-DRKI3342-NI-GVDWAGNISKZ   PIC S9(4) COMP.
END   *
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3342-GVDLFDNR   PIC S9(4) COMP.
             10 PO-DRKI3342-GVDABLAUF   PIC  X(010).
             10 PO-DRKI3342-GVDAKTVWERT   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3342-GVDAMTKZ   PIC  X(0012).
             10 PO-DRKI3342-GVDBEGINN   PIC  X(010).
             10 PO-DRKI3342-GVDBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3342-GVDBERECHDAT   PIC  X(010).
             10 PO-DRKI3342-GVDFLAECHE   PIC S9(9) COMP.
             10 PO-DRKI3342-GVDGESCHAEFTSZW   PIC  X(0002).
             10 PO-DRKI3342-GVDGUTHABEN   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3342-GVDLVART   PIC S9(4) COMP.
             10 PO-DRKI3342-GVDNJBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3342-GVDPRODKOMBI   PIC S9(9) COMP.
             10 PO-DRKI3342-GVDPRODUKT   PIC  X(0050).
             10 PO-DRKI3342-GVDPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI3342-GVDRENTEGARANT   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3342-GVDRENTENBEZUG   PIC  X(0001).
             10 PO-DRKI3342-GVDRENTEVORAUS   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3342-GVDSPARTE   PIC  X(0003).
             10 PO-DRKI3342-GVDSUMME   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3342-GVDTARIF   PIC  X(0050).
             10 PO-DRKI3342-GVDTARPROD-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI3342-GVDTARPROD-KTY   PIC S9(4) COMP.
             10 PO-DRKI3342-GVDWAGNISKZ   PIC S9(4) COMP.
             10 PO-DRKI3342-LETZTEAENDERUNG   PIC  X(0026).
END   *
 

