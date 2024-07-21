      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0943.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0943.CPv  $
      *
      *   Rev 2.2   Jun 08 2009 14:26:34   t13ae35
      *AAS00815: Neukundenwettbewerb -> Buchungsprogramm
      *
      *   Rev 2.1   Jun 05 2009 13:56:34   i13az23
      *AAS00815: Neukundenwettbewerb -> Buchungsprogramm
      *
      *   Rev 2.0   27 May 2009 09:22:42   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0943                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0305                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Ober                                             *
      *    DATUM:    27.05.2009                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AAS00815!27.05.09! 2.1! NEUERSTELLUNG             ! Ober     *
      * --------+--------+----+---------------------------+----------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0943                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0943.
          05 PI-DRVB0943-UFKT-CODE          PIC  9(02).
          05 PI-DRVB0943-MONAT              PIC  9(02).
          05 PI-DRVB0943-JAHR               PIC  9(04).
          05 PI-DRVB0943-KNR-WB-TYP         PIC  9(04).
      *
       01 PO-DRVB0943.

          05 PO-DRVB0943-RC                 PIC  X(003).
             88 PO-DRVB0943-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0943-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0943-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0943-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0943-DATEN.
             10 PO-DRVB0943-MA-ST-NR         PIC S9(009).
             10 PO-DRVB0943-KNR-WB-TYP       PIC S9(004).
             10 PO-DRVB0943-NKWBA-ANSPR-VOM  PIC  X(010).
             10 PO-DRVB0943-NKWBA-PART-VBNR  PIC S9(009).
             10 PO-DRVB0943-KNR-ANSPR-GRUND  PIC S9(004).
             10 PO-DRVB0943-KTY-WB-TYP       PIC S9(004).
             10 PO-DRVB0943-KTY-ANSPR-GRUND  PIC S9(004).
             10 PO-DRVB0943-NKWBA-LAST-UPDAT PIC  X(026).
             10 PO-DRVB0943-NKWBA-ANZ        PIC S9(004).
             10 PO-DRVB0943-NKWBA-EH-EING    PIC S9(9)V9  COMP-3.
             10 PO-DRVB0943-KTY-WAEHRUNG     PIC S9(004).
             10 PO-DRVB0943-KNR-WAEHRUNG     PIC S9(004).
             10 PO-DRVB0943-NKWBA-ANSPR-BETR PIC S9(9)V99  USAGE COMP-3.
             10 PO-DRVB0943-KTY-ANSPR-STATUS PIC S9(004).
             10 PO-DRVB0943-KNR-ANSPR-STATUS PIC S9(004).

      *
      ***** ENDE COPY-MEMBER DRVB0943 *****************************
      *
      *---------------------------------------------------------------*
