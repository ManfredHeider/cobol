      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.07.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0909.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0909.CPv  $
      *
      *   Rev 2.2   Sep 10 2012 13:08:14   t13ae30
      *ASL00462: Versorgungswerk 2012 - Anpassung F&A-SST
      *
      *   Rev 2.1   Sep 10 2008 11:51:14   t13ex43
      *ASL00309: PGM - Redesign Versorgungswerk (Batch)
      *
      *   Rev 2.0   Jul 17 2008 14:08:18   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0909                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0024                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    17.07.2008                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00309!17.07.08!2.1 ! NEU                       ! KLEEMEIER *
      * ASL00462!05.09.12!2.2 ! ERW. WG. NEUER ATTRIBUTE  ! WILLIG    *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0909 - ERMITTELN VSWZ-SATZ (GESAMTES TUPEL)   *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0909.
           05  PI-DRVB0909-DATEN.
               10  PI-DRVB0909-MA-ST-NR          PIC S9(09) COMP.
      *
       01  PO-DRVB0909.
           05  PO-DRVB0909-RC                    PIC  X(03).
               88  PO-DRVB0909-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0909-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB0909-NI.
               10  NI-DRVB0909-MA-ST-NR          PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-LAST-UPDATE  PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-ZUSAGE-DAT   PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-A-IG-DAT     PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-A-GESAMT     PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-G-DAT-VON    PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-G-STZ-JAHR   PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-G-VERS-SUM   PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-G-STZ-ANZ    PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-G-VERS-NR    PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-Z-VERS-SUM   PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-Z-VERS-NR    PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-Z-DAT-VON    PIC S9(04) BINARY.
               10  NI-DRVB0909-KTY-A-BERECHTIG   PIC S9(04) BINARY.
               10  NI-DRVB0909-KNR-A-BERECHTIG   PIC S9(04) BINARY.
               10  NI-DRVB0909-KTY-G-BERECHTIG   PIC S9(04) BINARY.
               10  NI-DRVB0909-KNR-G-BERECHTIG   PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-DBI-DAT      PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-DBI-KTO-NR   PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-G-RV-VERS-NR PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-G-RV-RENTE   PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-G-RV-ABF     PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-G-RV-BURENTE PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-Z-RV-VERS-NR PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-Z-RV-RENTE   PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-Z-RV-ABF     PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-Z-RV-BURENTE PIC S9(04) BINARY.
               10  NI-DRVB0909-KTY-Z-BERECHTIG   PIC S9(04) BINARY.
               10  NI-DRVB0909-KNR-Z-BERECHTIG   PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-G-VERSB-LV   PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-G-VERSB-RV   PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-Z-VERSB-LV   PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-Z-VERSB-RV   PIC S9(04) BINARY.
               10  NI-DRVB0909-VSWZ-AGI-KTO-NR   PIC S9(04) BINARY.
050912         10  NI-DRVB0909-VSWZ-AGI-KT-NR2   PIC S9(04) BINARY.
050912         10  NI-DRVB0909-VSWZ-GVA-VERSNR   PIC S9(04) BINARY.
050912         10  NI-DRVB0909-VSWZ-G-VA-VERSB   PIC S9(04) BINARY.
050912         10  NI-DRVB0909-VSWZ-G-VA-VSUM    PIC S9(04) BINARY.
050912         10  NI-DRVB0909-VSWZ-ZVA-VERSNR   PIC S9(04) BINARY.
050912         10  NI-DRVB0909-VSWZ-Z-VA-VERSB   PIC S9(04) BINARY.
050912         10  NI-DRVB0909-VSWZ-Z-VA-VSUM    PIC S9(04) BINARY.
050912         10  NI-DRVB0909-VSWZ-AVA-VERSNR   PIC S9(04) BINARY.
050912         10  NI-DRVB0909-VSWZ-A-VA-VERSB   PIC S9(04) BINARY.
050912         10  NI-DRVB0909-VSWZ-A-VA-VSUM    PIC S9(04) BINARY.
           05  PO-DRVB0909-DCL.
               10  PO-DRVB0909-MA-ST-NR          PIC S9(09)    BINARY.
               10  PO-DRVB0909-VSWZ-LAST-UPDATE  PIC  X(26).
               10  PO-DRVB0909-VSWZ-ZUSAGE-DAT   PIC  X(10).
               10  PO-DRVB0909-VSWZ-A-IG-DAT     PIC  X(10).
               10  PO-DRVB0909-VSWZ-A-GESAMT     PIC S9(09)V99 COMP-3.
               10  PO-DRVB0909-VSWZ-G-DAT-VON    PIC  X(10).
               10  PO-DRVB0909-VSWZ-G-STZ-JAHR   PIC  X(10).
               10  PO-DRVB0909-VSWZ-G-VERS-SUM   PIC S9(09)V99 COMP-3.
               10  PO-DRVB0909-VSWZ-G-STZ-ANZ    PIC S9(04)    BINARY.
               10  PO-DRVB0909-VSWZ-G-VERS-NR    PIC S9(15)    COMP-3.
               10  PO-DRVB0909-VSWZ-Z-VERS-SUM   PIC S9(09)V99 COMP-3.
               10  PO-DRVB0909-VSWZ-Z-VERS-NR    PIC S9(15)    COMP-3.
               10  PO-DRVB0909-VSWZ-Z-DAT-VON    PIC  X(10).
               10  PO-DRVB0909-KTY-A-BERECHTIG   PIC S9(04)    BINARY.
               10  PO-DRVB0909-KNR-A-BERECHTIG   PIC S9(04)    BINARY.
               10  PO-DRVB0909-KTY-G-BERECHTIG   PIC S9(04)    BINARY.
               10  PO-DRVB0909-KNR-G-BERECHTIG   PIC S9(04)    BINARY.
               10  PO-DRVB0909-VSWZ-DBI-DAT      PIC  X(10).
               10  PO-DRVB0909-VSWZ-DBI-KTO-NR   PIC  X(09).
               10  PO-DRVB0909-VSWZ-G-RV-VERS-NR PIC S9(15)    COMP-3.
               10  PO-DRVB0909-VSWZ-G-RV-RENTE   PIC S9(09)V99 COMP-3.
               10  PO-DRVB0909-VSWZ-G-RV-ABF     PIC S9(09)V99 COMP-3.
               10  PO-DRVB0909-VSWZ-G-RV-BURENTE PIC S9(09)V99 COMP-3.
               10  PO-DRVB0909-VSWZ-Z-RV-VERS-NR PIC S9(15)    COMP-3.
               10  PO-DRVB0909-VSWZ-Z-RV-RENTE   PIC S9(09)V99 COMP-3.
               10  PO-DRVB0909-VSWZ-Z-RV-ABF     PIC S9(09)V99 COMP-3.
               10  PO-DRVB0909-VSWZ-Z-RV-BURENTE PIC S9(09)V99 COMP-3.
               10  PO-DRVB0909-KTY-Z-BERECHTIG   PIC S9(04)    BINARY.
               10  PO-DRVB0909-KNR-Z-BERECHTIG   PIC S9(04)    BINARY.
               10  PO-DRVB0909-VSWZ-G-VERSB-LV   PIC S9(09)V99 COMP-3.
               10  PO-DRVB0909-VSWZ-G-VERSB-RV   PIC S9(09)V99 COMP-3.
               10  PO-DRVB0909-VSWZ-Z-VERSB-LV   PIC S9(09)V99 COMP-3.
               10  PO-DRVB0909-VSWZ-Z-VERSB-RV   PIC S9(09)V99 COMP-3.
               10  PO-DRVB0909-VSWZ-AGI-KTO-NR   PIC  X(10).
050912         10  PO-DRVB0909-VSWZ-AGI-KT-NR2   PIC  X(10).
050912         10  PO-DRVB0909-VSWZ-G-VA-VERS-NR PIC S9(15).
050912         10  PO-DRVB0909-VSWZ-G-VA-VERSB   PIC S9(09)V99 COMP-3.
050912         10  PO-DRVB0909-VSWZ-G-VA-VSUM    PIC S9(09)V99 COMP-3.
050912         10  PO-DRVB0909-VSWZ-Z-VA-VERS-NR PIC S9(15).
050912         10  PO-DRVB0909-VSWZ-Z-VA-VERSB   PIC S9(09)V99 COMP-3.
050912         10  PO-DRVB0909-VSWZ-Z-VA-VSUM    PIC S9(09)V99 COMP-3.
050912         10  PO-DRVB0909-VSWZ-A-VA-VERS-NR PIC S9(15).
050912         10  PO-DRVB0909-VSWZ-A-VA-VERSB   PIC S9(09)V99 COMP-3.
050912         10  PO-DRVB0909-VSWZ-A-VA-VSUM    PIC S9(09)V99 COMP-3.
      ****
      **** ENDE COPY DRVB0909 ****************************************
      ****
