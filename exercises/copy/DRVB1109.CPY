      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1109.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1109.CPv  $
      *
      *   Rev 2.1   Jul 08 2015 12:33:40   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - VB22
      *
      *   Rev 2.0   Jun 16 2015 13:09:14   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1109                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1109                   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: WALB                                            *
      *    DATUM:    22.06.2015                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01332!22.06.15!2.1 ! NEU                       ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1109
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1109.
           05 PI-DRVB1109-VB-NR                PIC  S9(009)    COMP.
      *
       01  PO-DRVB1109.
           05 PO-DRVB1109-RC                   PIC  X(003).
              88 PO-DRVB1109-RC-OK                VALUE 'OK '.
              88 PO-DRVB1109-RC-NOK               VALUE 'NOK'.
              88 PO-DRVB1109-RC-NOT-FOUND         VALUE 'NFD'.
              88 PO-DRVB1109-RC-ERR               VALUE 'ERR'.

           05 PO-DRVB1109-DATEN.
              10 PO-DRVB1109-EH-EIG-PZTR-NEU   PIC S9(09)V9(01) COMP-3.
              10 PO-DRVB1109-EQF-EIG-NEU       PIC S9(03)V9(02) COMP-3.
              10 PO-DRVB1109-EH-GES-PZTR-NEU   PIC S9(09)V9(01) COMP-3.
              10 PO-DRVB1109-B-EH-GES-PZTR-NEU PIC S9(09)V9(01) COMP-3.
              10 PO-DRVB1109-EQF-GES-NEU       PIC S9(03)V9(02) COMP-3.
              10 PO-DRVB1109-EH-EG-MX-PZTR-NEU PIC S9(09)V9(01) COMP-3.
              10 PO-DRVB1109-EH-GES-VERBL-NEU  PIC S9(09)V9(01) COMP-3.
              10 PO-DRVB1109-EH-GS-NT-GES-NEU  PIC S9(09)V9(01) COMP-3.
              10 PO-DRVB1109-EH-NEU-EIG-NP     PIC S9(09)V9(01) COMP-3.
              10 PO-DRVB1109-B-EH-NEU-EIG-NP   PIC S9(09)V9(01) COMP-3.
           05 PO-DRVB1109-NI.
              10 PO-DRVB1109-NI-EH-EIG-PZTR-NEU    PIC S9(04) COMP.
              10 PO-DRVB1109-NI-EQF-EIG-NEU        PIC S9(04) COMP.
              10 PO-DRVB1109-NI-EH-GES-PZTR-NEU    PIC S9(04) COMP.
              10 PO-DRVB1109-NI-B-EH-GES-PZ-NEU    PIC S9(04) COMP.
              10 PO-DRVB1109-NI-EQF-GES-NEU        PIC S9(04) COMP.
              10 PO-DRVB1109-NI-EH-EG-MX-PZ-NEU    PIC S9(04) COMP.
              10 PO-DRVB1109-NI-EH-GS-VERBL-NEU    PIC S9(04) COMP.
              10 PO-DRVB1109-NI-EH-GS-NT-GS-NEU    PIC S9(04) COMP.
              10 PO-DRVB1109-NI-EH-NEU-EIG-NP      PIC S9(04) COMP.
              10 PO-DRVB1109-NI-B-EH-NEU-EIG-NP    PIC S9(04) COMP.

      *
      ******* ENDE COPY-MEMBER **************************************

