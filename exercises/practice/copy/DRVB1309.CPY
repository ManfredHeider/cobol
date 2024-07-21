      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1309184                                     
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T13AE30                                          *
      *    DATUM:    14.04.2021                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * VLA-2401!14.04.21! ANPASSUNGEN  2021 - TEIL1      ! T13AE30  *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1309                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1309.
          05 PI-DRVB1309-PROGRAMM           PIC  X(008).
          05 PI-DRVB1309-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1309-MS-NR              PIC  9(004).
          05 PI-DRVB1309-DAT-VON            PIC  X(010).
          05 PI-DRVB1309-DAT-BIS            PIC  X(010).
          05 PI-DRVB1309-ED-STUFE-AB        PIC  9(004) COMP.
          05 PI-DRVB1309-KNR-VERTRIEB-GES   PIC S9(004) BINARY.
      *
       01 PO-DRVB1309.
          05 PO-DRVB1309-RC                 PIC  X(003).
             88 PO-DRVB1309-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1309-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1309-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1309-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB1309-RC-ERR                       VALUE 'ERR'.
          05 PO-DRVB1309-DATEN.
             10 PO-DRVB1309-VBNR            PIC  S9(009)    COMP.
             10 PO-DRVB1309-MS-NR           PIC  S9(004)    COMP.
             10 PO-DRVB1309-EH-GS-XX-GR     PIC  S9(009)V99 COMP-3.
      *
      ***** Ende-Copy-Member *****************************************

