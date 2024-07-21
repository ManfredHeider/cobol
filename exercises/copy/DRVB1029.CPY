      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1029.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1029.CPv  $
      *
      *   Rev 2.2   Sep 01 2011 11:48:48   t13ae14
      *ASL00390: Videosonderbonus Anpassung PMRW0018
      *
      *   Rev 2.1   Aug 19 2011 14:12:38   t13ae14
      *ASL00390: Videosonderbonus FTP und Load in die Tabelle VIDEO
      *A, Anpassung PMRW0018
      *
      *   Rev 2.0   Aug 16 2011 13:31:20   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1029                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: OBER                                             *
      *    DATUM:    16.08.2011                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00390!16.08.11!2.1 ! NEU                       ! OBER      *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1029 - ERMITTELN OFFENE ANSPRÜCHE AUS VIDEOA  *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1029.
           05  PI-DRVB1029-DATEN.
             10  PI-DRVB1029-UFKT-CODE         PIC  9(002).
             10  PI-DRVB1029-KNR-GESELL-TYP    PIC S9(004) COMP.
             10  PI-DRVB1029-KNR-ANSPR-STATUS  PIC S9(004) COMP.
             10  PI-DRVB1029-ANSPR-VOM-JJJJ    PIC  9(004).
             10  PI-DRVB1029-ANSPR-VOM-MM      PIC  9(002).
      *
       01  PO-DRVB1029.
           05  PO-DRVB1029-RC                  PIC  X(003).
               88  PO-DRVB1029-RC-OK                  VALUE 'OK '.
               88  PO-DRVB1029-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB1029-DATEN.
             10 PO-DRVB1029-MA-ST-NR           PIC S9(09)       COMP.
             10 PO-DRVB1029-VIDEOA-ANSPR-VOM   PIC  X(10).
             10 PO-DRVB1029-KNR-ANSPR-GRUND    PIC S9(04)       COMP.
             10 PO-DRVB1029-KTY-ANSPR-GRUND    PIC S9(04)       COMP.
             10 PO-DRVB1029-VIDEOA-LAST-UPDATE PIC  X(26).
             10 PO-DRVB1029-KTY-WAEHRUNG       PIC S9(04)       COMP.
             10 PO-DRVB1029-KNR-WAEHRUNG       PIC S9(04)       COMP.
             10 PO-DRVB1029-VIDEOA-ANSPR-BETRA PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1029-KTY-ANSPR-STATUS   PIC S9(04)       COMP.
             10 PO-DRVB1029-KNR-ANSPR-STATUS   PIC S9(04)       COMP.
      ****
      **** ENDE COPY DRVB0875 ****************************************
      ****
