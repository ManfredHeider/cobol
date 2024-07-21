      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  06.12.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3538.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3538.CPv  $
      *
      *   Rev 2.4   Nov 24 2020 08:32:36   u130685
      *AKI01805: G48_Vertrag OZP/Copy
      *
      *   Rev 2.3   Feb 19 2019 06:47:30   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.2   Feb 18 2019 16:31:34   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.1   Jan 30 2019 11:20:56   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.0   Dec 06 2018 11:40:40   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3538                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0519 G48_VERTRAG                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01652!03.12.18!    ! Neuerstellung             ! who      *
      *---------!--------!----!---------------------------!----------*
      * AKI01805!09.11.20!    ! NEUES ATTRIBUT ESAFE      ! ATAR     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3538
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3538.
           05 PI-DRKI3538-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3538-UFKT-FIRST             VALUE 1.
              88 PI-DRKI3538-UFKT-NEXT              VALUE 2.
              88 PI-DRKI3538-UFKT-QUIT              VALUE 3.

           05  PI-DRKI3538-KEY.
               10 PI-DRKI3538-G48MANDANT       PIC S9(9) COMP.

      *
       01  PO-DRKI3538.
           05 PO-DRKI3538-RC                   PIC  X(003).
              88 PO-DRKI3538-RC-OK                  VALUE 'OK '.
              88 PO-DRKI3538-RC-LEER                VALUE 'LER'.
              88 PO-DRKI3538-RC-EOF                 VALUE 'EOF'.
      *
           05 PO-DRKI3538-DATEN.
              10 PO-DRKI3538-G48VVERTRAGSNR       PIC X(15).
              10 PO-DRKI3538-G48VMANDANT          PIC S9(9) USAGE COMP.
              10 PO-DRKI3538-G48VVBNR             PIC S9(9) USAGE COMP.
              10 PO-DRKI3538-G48VVERTGBEGINN      PIC X(8).
              10 PO-DRKI3538-G48VVERTENDE         PIC X(8).
              10 PO-DRKI3538-G48VINFOSPRREKZ      PIC X(1).
              10 PO-DRKI3538-G48VINFOSPERDAT      PIC X(8).
              10 PO-DRKI3538-G48VVERFUEGUNG       PIC X(1).
              10 PO-DRKI3538-G48VANACHLASSKZ      PIC X(1).
              10 PO-DRKI3538-G48VTELBANKING       PIC X(1).
              10 PO-DRKI3538-G48VONLBANKING       PIC X(1).
              10 PO-DRKI3538-G48VPERSONLFDNR      PIC S9(4) USAGE COMP.
              10 PO-DRKI3538-G48VNACHNAME         PIC X(35).
              10 PO-DRKI3538-G48VVORNAME          PIC X(35).
              10 PO-DRKI3538-G48VGEBDAT           PIC X(8).
              10 PO-DRKI3538-G48VGESCHLECHT       PIC X(1).
              10 PO-DRKI3538-G48VANREDE           PIC X(1).
              10 PO-DRKI3538-G48VSTR              PIC X(28).
              10 PO-DRKI3538-G48VHAUSNR           PIC X(9).
              10 PO-DRKI3538-G48VPOSTFACH         PIC X(9).
              10 PO-DRKI3538-G48VPLZ              PIC X(9).
              10 PO-DRKI3538-G48VORT              PIC X(28).
              10 PO-DRKI3538-G48VLKZ              PIC X(3).
              10 PO-DRKI3538-G48VTELVOR           PIC X(6).
              10 PO-DRKI3538-G48VTESLNR           PIC X(9).
              10 PO-DRKI3538-G48VDEPOTKZ          PIC X(1).
              10 PO-DRKI3538-G48VBLZ              PIC X(8).
              10 PO-DRKI3538-G48VDELKZ            PIC X(1).
              10 PO-DRKI3538-G48VBIC              PIC X(11).
              10 PO-DRKI3538-G48VKNDNAME          PIC X(28).
              10 PO-DRKI3538-G48VFILIALEART       PIC X(35).
              10 PO-DRKI3538-G48VFILIALENAME      PIC X(35).
              10 PO-DRKI3538-G48VFILIALESTR       PIC X(35).
              10 PO-DRKI3538-G48VFILIALEORT       PIC X(35).
              10 PO-DRKI3538-G48VESAFE            PIC X(1).




