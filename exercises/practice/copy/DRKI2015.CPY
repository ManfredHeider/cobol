      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2015.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2015.CPv  $
      *
      *   Rev 2.4   Aug 14 2007 12:03:56   t13ex48
      *AKI00662: Anpassung KoKÜ an OKI:Ticket-UP
      *
      *   Rev 2.3   04 Jun 2007 18:00:40   t13ex43
      *FKI01091: Falsches Eigenvermittelt-Kz. in KUNDENUEBERTRAGUNG
      *
      *   Rev 2.2   21 May 2007 14:56:58   t13ex43
      *AKI00641: Erweiterung DRKI2015 um KKVLID
      *
      *   Rev 2.1   15 Mar 2007 10:34:06   t13ex43
      *AKI00641: PGM2 - Anpassung KÜ Deutsche Bank für OKI
      *
      *   Rev 2.0   01 Feb 2007 15:11:02   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2015                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0203                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    02.02.2007                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00641!02.02.07!2.1 ! NEU                       ! KLEEMEIER *
210507* AKI00641!21.05.07!2.2 ! KKVLID HINZU              ! KLEEMEIER *
040607* FKI01091!04.06.07!2.3 ! KKVLID ENTFÄLLT, EIGEN-   ! KLEEMEIER *
040607*         !        !    ! VERM-ALT HINZU            !           *
      * --------+--------+----+---------------------------+-----------*
010807* AKI00662!01.08.07!2.4 ! KKVLID wieder aufgenommen ! HSE       *
      * --------+--------+----+---------------------------+-----------*
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI2015 - KUNDENÜBERTRAGUNG                      *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI2015.
           05  PI-DRKI2015-DATEN.
               10  PI-DRKI2015-HOSTVERTRAGID     PIC  X(013).
               10  PI-DRKI2015-VBPERSONENID-ALT  PIC  X(013).
               10  PI-DRKI2015-VBPERSONENID-NEU  PIC  X(013).
               10  PI-DRKI2015-GUELTIG-AB-DAT    PIC  X(010).
               10  PI-DRKI2015-EIGEN-VERM-KZ     PIC  X(001).
               10  PI-DRKI2015-ERSTELLTVON       PIC  X(030).
               10  PI-DRKI2015-AENDGRUND-KNR     PIC S9(004) COMP.
040607*210507  10  PI-DRKI2015-KKVLID            PIC  X(013).
010807         10  PI-DRKI2015-KKVLID            PIC  X(013).
040607         10  PI-DRKI2015-EIGEN-VERM-ALT    PIC  X(001).
      *
       01  PO-DRKI2015.
           05  PO-DRKI2015-RC                    PIC  X(003).
               88  PO-DRKI2015-RC-OK                  VALUE 'OK '.
               88  PO-DRKI2015-RC-NOK                 VALUE 'NOK'.
           05  PO-DRKI2015-DATEN.
               10  PO-DRKI2015-FEHLER-KNR        PIC S9(004) COMP.
      ****
      **** ENDE COPY DRKI2015 ****************************************
      ****
