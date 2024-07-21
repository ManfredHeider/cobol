      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0630.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0630.CPv  $
      *
      *   Rev 2.2   Sep 04 2003 13:04:38   t13ex22
      *AVB00701:Dialog VB44 - Pflege Insolvenzkonten; 
      *Anzeige BV-Schutz in UU11 und VB21
      *
      *   Rev 2.1   Dec 19 2002 14:41:08   t13ae30
      *FVB00513 - Kapitänsclub abgebrochen
      *
      *   Rev 2.0   Dec 04 2002 12:21:26   t13ex15
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRVB0630                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0630                   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: TH.WILLIG                                       *
      *    DATUM:    19.12.2002                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FVB00513!19.12.02!    ! NEU                       !TH.WILLIG *
      * AVB00701!22.08.03!    ! RC-NFD ERGÄNZT            !MUND      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0630
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0630.
           05  PI-DRVB0630-MA-ST-NR               PIC  9(009) COMP.
           05  PI-DRVB0630-KTY-MAST-TYP           PIC S9(004) COMP.
           05  PI-DRVB0630-KNR-MAST-TYP           PIC S9(004) COMP.
      *
       01  PO-DRVB0630.
           05  PO-DRVB0630-RC                     PIC  X(003).
               88 PO-DRVB0630-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0630-RC-LEER                      VALUE 'LER'.
               88 PO-DRVB0630-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0630-RC-EOF                       VALUE 'EOF'.
220803         88 PO-DRVB0630-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0630-DATEN.
               10 PO-DRVB0630-MA-ST-NR            PIC S9(9) USAGE COMP.
               10 PO-DRVB0630-MAST-DAT-VON        PIC X(26).
               10 PO-DRVB0630-KTY-MAST-TYP        PIC S9(4) USAGE COMP.
               10 PO-DRVB0630-KNR-MAST-TYP        PIC S9(4) USAGE COMP.
               10 PO-DRVB0630-MAST-LAST-UPDATE    PIC X(26).
               10 PO-DRVB0630-MAST-DAT-BIS        PIC X(10).
      *
      ******* ENDE COPY-MEMBER **************************************
