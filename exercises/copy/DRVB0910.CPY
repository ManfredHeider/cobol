      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.07.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0910.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0910.CPv  $
      *
      *   Rev 2.1   Jul 30 2008 10:08:54   t13ae13
      *FVB00669: Korrekturen im VB-System - VBA1/VBAK (swl)
      *
      *   Rev 2.0   Jul 28 2008 12:10:28   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0910                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0013                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    28.07.2008                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FVB00669!28.07.08!2.1 ! NEUERSTELLUNG             ! S. Walb  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0910:                                *
      *    ** WELCHER VB-VERTRAG ZUM STICHTAG JE VB                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0910.
           05  PI-DRVB0910-KEY.
               10 PI-DRVB0910-MA-ST-NR        PIC S9(009) BINARY.
               10 PI-DRVB0910-STICHTAG-ISO    PIC  X(010).
      *
       01  PO-DRVB0910.
           05  PO-DRVB0910-RC                 PIC  X(003).
               88 PO-DRVB0910-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0910-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0910-AV-DAT-VON         PIC  X(010).
      *
      ***************** ENDE COPY-MEMBER DRVB0910 *******************
