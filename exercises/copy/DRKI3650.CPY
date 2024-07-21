      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3650.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3650.CPv  $
      *
      *   Rev 2.2   Jun 05 2019 11:18:40   t13ex78
      *AKI01669: OZP G67_VERTRAG
      *
      *   Rev 2.1   Apr 15 2019 11:26:48   t13ex78
      *AKI01669: OZP G67_VERTRAG
      *
      *   Rev 2.0   Mar 22 2019 15:35:24   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3650                                      *
      *    ----------------------------------------------------------*
      *    Cursor Full SELECT ZXKI0532 G67_VERTRAG                   *
      *    ----------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01669!26.03.18!2.0 ! Neuerstellung             ! tfl      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3650
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3650.
           05 PI-DRKI3650-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3650-UFKT-FIRST             VALUE 1.
              88 PI-DRKI3650-UFKT-NEXT              VALUE 2.
              88 PI-DRKI3650-UFKT-QUIT              VALUE 3.
 
           05  PI-DRKI3650-KEY.
               10 PI-DRKI3650-G67MANDANT       PIC S9(9) COMP.
 
      *
       01  PO-DRKI3650.
           05 PO-DRKI3650-RC                   PIC  X(003).
              88 PO-DRKI3650-RC-OK                  VALUE 'OK '.
              88 PO-DRKI3650-RC-LEER                VALUE 'LER'.
              88 PO-DRKI3650-RC-EOF                 VALUE 'EOF'.
      *
           05 PO-DRKI3650-DATEN.
              10 PO-DRKI3650-G67VERTRAGSNR    PIC X(15).
              10 PO-DRKI3650-G67MANDANT       PIC S9(9) COMP.
              10 PO-DRKI3650-G67ABSCHLUSSVBNR PIC S9(9) COMP.
              10 PO-DRKI3650-G67VRTRBEGINN    PIC X(8).
              10 PO-DRKI3650-G67VRTRENDE      PIC X(8).
              10 PO-DRKI3650-G67DLBETRAG      PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3650-G67KREDITZUS     PIC X(8).
              10 PO-DRKI3650-G67KREDITAUSZ    PIC X(8).
              10 PO-DRKI3650-G67KREDITART     PIC X(6).
              10 PO-DRKI3650-G67SALDO         PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3650-G67ZINSSATZ      PIC S9(2)V9(4)  COMP-3.
              10 PO-DRKI3650-G67RATE          PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3650-G67FAELLIGKEIT   PIC X(8).
              10 PO-DRKI3650-G67SICHERHEIT    PIC X(1).
              10 PO-DRKI3650-G67RESTLAUFZEIT  PIC X(3).
              10 PO-DRKI3650-G67STATUS        PIC X(1).
      *
      ******* ENDE COPY-MEMBER ***************************************


