      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0375.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0375.CPv  $
      *
      *   Rev 2.4   Jul 30 2015 09:05:30   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e (PG11, PG12, PG14, PF11)
      *
      *   Rev 2.3   Dec 17 2013 10:44:32   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PG14
      *
      *   Rev 2.2   03 Nov 2006 09:55:48   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Jun 10 2005 09:48:38   t13ex44
      *APA00353: Sortierung der Datensätze im Provisionseingang
      *
      *   Rev 2.0   Feb 27 1999 14:28:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:25:22   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.1   Apr 09 1997 16:48:12   T13AE31
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0375                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0375                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    18.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!18.07.97! 01 ! NEU                       ! D.WILHELM*
      * AFPA0012!15.10.97! 1.1! PRODSCHL  MIT VORZEICHEN  ! D.WILHELM*
      * AFPA0012!26.02.98! 1.2! PRODSL-KZ FÜR PLUS-ZEICHEN!          *
      *         !        !    ! BEI MEHR ALS 3 PRODUKTEN  ! D.WILHELM*
      * APA00353!02.06.05! 2.1! SORTIERUNG MITTELS FCS    ! SCHEIDEL *
      * APA00452!01.11.06! 2.2! UMBENENNUNG WG. 3-STELL.  ! WANG     *
      *         !        !    ! PRODUKTE                  !          *
      * APA01526!15.07.13! 2.3! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA01932!22.07.15! 2.4! ERWEITERUNG PO-BEREICH    ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0375
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0375 DES BUFFER-OZP ZYPA0005 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PG14, Auswahlliste nPM
      *
      ****************************************************************
      *
       01  PI-DRPA0375.
           05  PI-DRPA0375-UFKT                PIC  9(02).
           05  PI-DRPA0375-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA0375-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA0375-PDTA-ID             PIC S9(15)V COMP-3.
           05  PI-DRPA0375-PGEB-GES            PIC S9(04) COMP.
           05  PI-DRPA0375-PMPP-PGV            PIC S9(04) COMP.
020605     05  PI-DRPA0375-SORTIERUNG          PIC  X(01).
      *
       01  PO-DRPA0375.
           05  PO-DRPA0375-RC                  PIC  X(03).
               88 PO-DRPA0375-RC-OK                 VALUE 'OK '.
               88 PO-DRPA0375-RC-EOF                VALUE 'EOF'.
               88 PO-DRPA0375-RC-LEER               VALUE 'LER'.
           05  PO-DRPA0375-ANZAHL-ERG-ZEILEN  PIC  9(05).
           05  PO-DRPA0375-ERGEBNISZEILE.
               10 PO-DRPA0375-PMPG-ID         PIC S9(15)V COMP-3.
               10 PO-DRPA0375-PMPG-VERSION    PIC S9(04) COMP.
               10 PO-DRPA0375-VNR             PIC  X(15).
               10 PO-DRPA0375-VSTAT           PIC  X(04).
               10 PO-DRPA0375-PROD-PROD-SCHL-1 PIC S9(04) BINARY.
               10 PO-DRPA0375-PROD-PROD-SCHL-2 PIC S9(04) BINARY.
               10 PO-DRPA0375-PROD-PROD-SCHL-3 PIC S9(04) BINARY.
               10 PO-DRPA0375-PG-GSMT-PROV     PIC S9(9)V9(2) COMP-3.
               10 PO-DRPA0375-PGV              PIC S9(04) COMP.
               10 PO-DRPA0375-FAELLIG-VON      PIC  X(10).
               10 PO-DRPA0375-FAELLIG-BIS      PIC  X(10).
               10 PO-DRPA0375-PRODSL-KZ        PIC  X.
150713         10 PO-DRPA0375-PMPE-UNR         PIC S9(04) COMP.
150713         10 PO-DRPA0375-PMPE-SCHICHT-NR  PIC S9(04) COMP.
220715         10 PO-DRPA0375-PG-UST-BETRAG    PIC S9(9)V9(2) COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

