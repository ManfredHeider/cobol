      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  14.01.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3761.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3761.CPv  $
      *
      *   Rev 2.3   May 12 2020 07:41:58   t13ex78
      *AKI01731: List OZP G66_KUNDE
      *
      *   Rev 2.2   May 12 2020 07:37:30   t13ex78
      *AKI01731: List OZP G66_KUNDE
      *
      *   Rev 2.1   Feb 25 2020 13:01:38   t13ex78
      *AKI01731: List OZP G66_KUNDE
      *
      *   Rev 2.0   Jan 14 2020 12:36:58   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3761                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0537 G66_KUNDE                      *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01731!14.01.20!2.0 ! Neuerstellung             ! t13ex78   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3761
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
      *
       01  PI-DRKI3761.
           05 PI-DRKI3761-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3761-UFKT-FIRST        VALUE 1.
              88 PI-DRKI3761-UFKT-NEXT         VALUE 2.
              88 PI-DRKI3761-UFKT-QUIT         VALUE 3.
 
           05  PI-DRKI3761-KEY.
               10 PI-DRKI3761-G66PMANDANT      PIC S9(9) BINARY.
 
      *
       01  PO-DRKI3761.
           05 PO-DRKI3761-RC                   PIC  X(003).
              88 PO-DRKI3761-RC-OK             VALUE 'OK '.
              88 PO-DRKI3761-RC-LEER           VALUE 'LER'.
              88 PO-DRKI3761-RC-EOF            VALUE 'EOF'.
      *
           05 PO-DRKI3761-DATEN.
              10 PO-DRKI3761-G66KVERTRAGSNR    PIC X(15).
              10 PO-DRKI3761-G66KLFDNR         PIC S9(4) BINARY.
              10 PO-DRKI3761-G66KNAME          PIC X(35).
              10 PO-DRKI3761-G66KVORNAME       PIC X(35).
              10 PO-DRKI3761-G66KGEBURTSDATUM  PIC X(08).
              10 PO-DRKI3761-G66KSTRHNR        PIC X(40).
              10 PO-DRKI3761-G66KPLZ           PIC X(9).
              10 PO-DRKI3761-G66KORT           PIC X(30).
              10 PO-DRKI3761-G66KISOCODE       PIC X(3).
              10 PO-DRKI3761-G66KANREDE        PIC X(1).
 
      ******* ENDE COPY-MEMBER ***************************************


