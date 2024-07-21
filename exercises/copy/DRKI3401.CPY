      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  31.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3401.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3401.CPv  $
      *
      *   Rev 2.1   Oct 22 2018 11:48:36   t13ex39
      *AKI01615: Generali Invest  OZPs  WHO   
      *
      *   Rev 2.0   Aug 31 2018 16:01:00   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3401                                      *
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
      *    FÜR DIE FUNKTION DRKI3401
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3401.
           05 PI-DRKI3401-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3401-UFKT-FIRST             VALUE 1.
              88 PI-DRKI3401-UFKT-NEXT              VALUE 2.
              88 PI-DRKI3401-UFKT-QUIT              VALUE 3.

           05  PI-DRKI3401-KEY.
               10 PI-DRKI3401-G42ZKVERTRAGSNR    PIC X(15).

      *
       01  PO-DRKI3401.
           05 PO-DRKI3401-RC                   PIC  X(003).
              88 PO-DRKI3401-RC-OK                  VALUE 'OK '.
              88 PO-DRKI3401-RC-LEER                VALUE 'LER'.
              88 PO-DRKI3401-RC-EOF                 VALUE 'EOF'.
      *
           05 PO-DRKI3401-DATEN.
              10 PO-DRKI3401-G42ZKVERTRAGSNR      PIC X(15).
              10 PO-DRKI3401-G42ZKLFDNR           PIC S9(9) USAGE COMP.
              10 PO-DRKI3401-G42ZKNACHNAME        PIC X(35).
              10 PO-DRKI3401-G42ZKVORNAME         PIC X(35).
              10 PO-DRKI3401-G42ZKGEBDAT          PIC X(8).
              10 PO-DRKI3401-G42ZKGESCHLECHT      PIC X(1).
              10 PO-DRKI3401-G42ZKANREDE          PIC X(1).
              10 PO-DRKI3401-G42ZKSTRHNR          PIC X(40).
              10 PO-DRKI3401-G42ZKPLZ             PIC X(9).
              10 PO-DRKI3401-G42ZKORT             PIC X(40).
              10 PO-DRKI3401-G42ZKLKZ             PIC X(3).
      *
      ******* ENDE COPY-MEMBER ***************************************


