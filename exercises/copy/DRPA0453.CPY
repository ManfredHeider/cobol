      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0453.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0453.CPv  $
      *
      *   Rev 2.4   Apr 07 2016 10:02:50   t13ae46
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlung
      *
      *   Rev 2.3   15 Nov 2006 15:46:54   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   13 Nov 2006 13:59:56   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Apr 27 2000 23:00:12   T13AE18
      *APA00067
      *
      *
      *   Rev 2.0   Feb 27 1999 14:29:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:12:46   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   23 Jul 1997 12:38:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0453                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0453                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    18.08.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!18.08.97! 01 ! NEU                       ! D.WILHELM*
      * AFPA0012!01.09.97! 1.1! PROV-GESAMT und PROV-SATZ ! D.WILHELM*
      *         !        !    ! Namensänderung            !          *
      * AFPA0012!13.10.97! 1.2! PG-DATEN -> PG-PROD-DATEN ! D.WILHELM*
      *         !        !    ! Namensänderung            !          *
      * AFPA0067!27.04.00! 2.1! ERWEITERT UM PPFL. SUMME  ! BOHLINGER*
      * APA00452!13.11.06! 2.2! ÄNDERUNG 3-STELL. PRODUKTE! WANG     *
      *         !        ! 2.3! ÄNDERUNG                  !          *
      * APA02220!21.03.16! 2.4! ÄNDERUNG FORMAT %-SÄTZE   ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0453
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0453 DES BUFFER-OZP ZYPA0007 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PG16, nPM Bild 2
      *
      ****************************************************************
      *
       01  PI-DRPA0453.
           05  PI-DRPA0453-UFKT                PIC  9(02).
           05  PI-DRPA0453-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA0453-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA0453-PMPG-ID             PIC  9(15).
      *
       01  PO-DRPA0453.
           05  PO-DRPA0453-RC                  PIC  X(03).
               88 PO-DRPA0453-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0453-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0453-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0453-ANZAHL-ERG-ZEILEN   PIC  9(05).
           05  PO-DRPA0453-ERGEBNISZEILE.
               10 PO-DRPA0453-PMPP-ID             PIC  9(15).
               10 PO-DRPA0453-PGEB-GES            PIC  9(04).
131106*        10 PO-DRPA0453-PRODSCHL            PIC  S9(04).
151106*        10 PO-DRPA0453-PROD-PROD-SCHL      PIC  S9(04) BINARY.
151106         10 PO-DRPA0453-PROD-PROD-SCHL      PIC  S9(04).
               10 PO-DRPA0453-PGV                 PIC  9(04).
               10 PO-DRPA0453-PMPP-PPFL-BEITRAG   PIC  S9(11)V9(2).
270400         10 PO-DRPA0453-PMPP-PPFL-SUMME     PIC  S9(11)V9(2).
210316*        10 PO-DRPA0453-PMPP-PG-PROV-STZ    PIC  9(03)V999.
210316         10 PO-DRPA0453-PMPP-PG-PROV-STZ    PIC  9(04)V9(04).
               10 PO-DRPA0453-PMPP-PG-GSMT-PROV   PIC  S9(15)V9(2).
               10 PO-DRPA0453-PKON-KOND-NR        PIC  9(04).
               10 PO-DRPA0453-PMPP-PG-PROD-DATEN  PIC  X(30).
      *
      ******* ENDE COPY-MEMBER **************************************

