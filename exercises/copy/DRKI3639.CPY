      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  15.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3639.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3639.CPv  $
      *
      *   Rev 2.2   Jun 07 2019 08:23:10   t13ex78
      *AKI01669: OZP G67_KDETICKETS
      *
      *   Rev 2.1   Mar 19 2019 12:43:50   t13ex78
      *AKI01669: OZP G67_KDETICKETS
      *
      *   Rev 2.0   Mar 15 2019 15:51:00   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3639                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0531 G67_KDETICKETS                 *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01669!18.03.19!2.0 ! Neuerstellung             ! tfl       *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3639
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3639.
           05 PI-DRKI3639-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3639-UFKT-FIRST        VALUE 1.
              88 PI-DRKI3639-UFKT-NEXT         VALUE 2.
              88 PI-DRKI3639-UFKT-QUIT         VALUE 3.
 
           05  PI-DRKI3639-KEY.
               10 PI-DRKI3639-G67TVERTRAGSNR   PIC S9(9) COMP.
 
      *
       01  PO-DRKI3639.
           05 PO-DRKI3639-RC                   PIC  X(003).
              88 PO-DRKI3639-RC-OK             VALUE 'OK '.
              88 PO-DRKI3639-RC-LEER           VALUE 'LER'.
              88 PO-DRKI3639-RC-EOF            VALUE 'EOF'.
      *
           05 PO-DRKI3639-DATEN.
              10 PO-DRKI3639-G67TLFDNR         PIC S9(4) USAGE COMP.
              10 PO-DRKI3639-G67TNACHNAME      PIC X(30).
              10 PO-DRKI3639-G67TVORNAME       PIC X(30).
              10 PO-DRKI3639-G67TGEBDAT        PIC X(10).
              10 PO-DRKI3639-G67TSTRHNR        PIC X(40).
              10 PO-DRKI3639-G67TPLZ           PIC X(9).
              10 PO-DRKI3639-G67TORT           PIC X(30).
              10 PO-DRKI3639-G67TLAND          PIC X(3).
              10 PO-DRKI3639-G67TANREDE        PIC X(1).
              10 PO-DRKI3639-G67TTICKETID      PIC S9(18)V USAGE COMP-3.
      *
      ******* ENDE COPY-MEMBER ***************************************


