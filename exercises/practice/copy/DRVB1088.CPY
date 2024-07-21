      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1088.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1088.CPv  $
      *
      *   Rev 2.2   Jul 10 2015 08:52:10   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - VBI/2 
      *
      *   Rev 2.1   Jun 03 2015 12:41:04   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - VBI
      *
      *   Rev 2.0   Mar 24 2015 10:16:08   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1088                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0038                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    24.03.2015                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01332!24.03.15!2.1 ! NEUERSTELLUNG             ! S. Walb  *
      * AVB01332!01.07.15!2.2 ! Ebene entfällt            ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1088                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1088.
          05 PI-DRVB1088-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1088-KNR-WB-TYP         PIC  S9(004) COMP.
          05 PI-DRVB1088-PARTEH-BETR        PIC  S9(009) COMP.
          05 PI-DRVB1088-DATUM-VON          PIC  X(010).
          05 PI-DRVB1088-DATUM-BIS          PIC  X(010).
      *
       01 PO-DRVB1088.

          05 PO-DRVB1088-RC                 PIC  X(003).
             88 PO-DRVB1088-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1088-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1088-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1088-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB1088-DATEN.
             10 PO-DRVB1088-PARTEH-UNTST       PIC  S9(009) COMP.
             10 PO-DRVB1088-PARTEH-EH          PIC  S9(009)V9 COMP-3.
             10 PO-DRVB1088-PARTEH-URSPR-STRUK PIC   X(001).
010715*      10 PO-DRVB1088-EBENE              PIC   X(001).
      *
      ***** ENDE COPY-MEMBER DRVB1088 *****************************


