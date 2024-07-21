      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  31.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3399.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3399.CPv  $
      *
      *   Rev 2.1   Oct 22 2018 11:48:12   t13ex39
      *AKI01615: Generali Invest  OZPs  WHO   
      *
      *   Rev 2.0   Aug 31 2018 16:00:38   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3399                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0507 G42_VERTRAG                      *
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
      *    FÜR DIE FUNKTION DRKI3399
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3399.
           05 PI-DRKI3399-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3399-UFKT-FIRST             VALUE 1.
              88 PI-DRKI3399-UFKT-NEXT              VALUE 2.
              88 PI-DRKI3399-UFKT-QUIT              VALUE 3.

           05  PI-DRKI3399-KEY.
               10 PI-DRKI3399-G79MANDANT       PIC S9(9) COMP.

      *
       01  PO-DRKI3399.
           05 PO-DRKI3399-RC                   PIC  X(003).
              88 PO-DRKI3399-RC-OK                  VALUE 'OK '.
              88 PO-DRKI3399-RC-LEER                VALUE 'LER'.
              88 PO-DRKI3399-RC-EOF                 VALUE 'EOF'.
      *
           05 PO-DRKI3399-DATEN.
              10 PO-DRKI3399-G42VVERTRAGSNR       PIC X(15).
              10 PO-DRKI3399-G42VMANDANT          PIC S9(9) USAGE COMP.
              10 PO-DRKI3399-G42VVBNR             PIC S9(9) USAGE COMP.
              10 PO-DRKI3399-G42VVRTRGBEGINN      PIC X(8).
              10 PO-DRKI3399-G42VDEPOTSTATUS      PIC X(1).
              10 PO-DRKI3399-G42VINFOSPRREKZ      PIC X(1).
              10 PO-DRKI3399-G42VINFOSPERDAT      PIC X(8).
              10 PO-DRKI3399-G42VANACHLASSKZ      PIC X(1).
              10 PO-DRKI3399-G42VTELBANKING       PIC X(1).
              10 PO-DRKI3399-G42VONLBANKING       PIC X(1).
              10 PO-DRKI3399-G42VNACHNAME         PIC X(35).
              10 PO-DRKI3399-G42VVORNAME          PIC X(35).
              10 PO-DRKI3399-G42VGEBDAT           PIC X(8).
              10 PO-DRKI3399-G42VGESCHLECHT       PIC X(1).
              10 PO-DRKI3399-G42VANREDE           PIC X(1).
              10 PO-DRKI3399-G42VSTRHNR           PIC X(40).
              10 PO-DRKI3399-G42VPLZ              PIC X(9).
              10 PO-DRKI3399-G42VORT              PIC X(40).
              10 PO-DRKI3399-G42VLKZ              PIC X(3).
      *
      ******* ENDE COPY-MEMBER ***************************************


