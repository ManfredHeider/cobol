      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.10.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3503.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3503.CPv  $
      *
      *   Rev 2.1   Oct 22 2018 11:48:48   t13ex39
      *AKI01615: Generali Invest  OZPs  WHO   
      *
      *   Rev 2.0   Oct 09 2018 07:14:36   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3503                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0508 G42_ZUS_KUNDEN                 *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01615!03.09.18!    ! Neuerstellung             ! who      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3503
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3503.
           05 PI-DRKI3503-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3503-UFKT-FIRST             VALUE 1.
              88 PI-DRKI3503-UFKT-NEXT              VALUE 2.
              88 PI-DRKI3503-UFKT-QUIT              VALUE 3.

           05  PI-DRKI3503-KEY.
               10 PI-DRKI3503-G42ZKVERTRAGSNR    PIC X(15).

      *
       01  PO-DRKI3503.
           05 PO-DRKI3503-RC                   PIC  X(003).
              88 PO-DRKI3503-RC-OK                  VALUE 'OK '.
              88 PO-DRKI3503-RC-LEER                VALUE 'LER'.
              88 PO-DRKI3503-RC-EOF                 VALUE 'EOF'.
      *
           05 PO-DRKI3503-DATEN.
              10 PO-DRKI3503-G42ZKVERTRAGSNR      PIC X(15).
              10 PO-DRKI3503-G42ZKLFDNR           PIC S9(9) USAGE COMP.
              10 PO-DRKI3503-G42ZKNACHNAME        PIC X(35).
              10 PO-DRKI3503-G42ZKVORNAME         PIC X(35).
              10 PO-DRKI3503-G42ZKGEBDAT          PIC X(8).
              10 PO-DRKI3503-G42ZKGESCHLECHT      PIC X(1).
              10 PO-DRKI3503-G42ZKANREDE          PIC X(1).
              10 PO-DRKI3503-G42ZKSTRHNR          PIC X(40).
              10 PO-DRKI3503-G42ZKPLZ             PIC X(9).
              10 PO-DRKI3503-G42ZKORT             PIC X(40).
              10 PO-DRKI3503-G42ZKLKZ             PIC X(3).
      *
      ******* ENDE COPY-MEMBER ***************************************


