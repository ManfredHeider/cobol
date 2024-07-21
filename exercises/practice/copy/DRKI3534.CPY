      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.12.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3534.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3534.CPv  $
      *
      *   Rev 2.4   Nov 24 2020 08:32:02   u130685
      *AKI01805: G48_Vertrag OZP/Copy
      *
      *   Rev 2.3   Feb 19 2019 06:46:28   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.2   Feb 18 2019 16:30:28   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.1   Jan 30 2019 11:20:00   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.0   Dec 6 2018 15:46:21 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3534                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3534                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 14:04:38 / U130685
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      *---------!--------!--------------------------------!----------*
      * AKI01805!13.11.20!    ! NEUES ATTRIBUT ESAFE      ! ATAR     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3534
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3534.
           05  PI-DRKI3534-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3534-G48VVERTRAGSNR   PIC  X(0015).
END   *

           05  PI-DRKI3534-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3534-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3534-G48VANACHLASSKZ   PIC  X(0001).
             10 PI-DRKI3534-G48VANREDE   PIC  X(0001).
             10 PI-DRKI3534-G48VBIC   PIC  X(0011).
             10 PI-DRKI3534-G48VBLZ   PIC  X(0008).
             10 PI-DRKI3534-G48VDELKZ   PIC  X(0001).
             10 PI-DRKI3534-G48VDEPOTKZ   PIC  X(0001).
             10 PI-DRKI3534-G48VESAFE   PIC  X(0001).
             10 PI-DRKI3534-G48VFILIALEART   PIC  X(0035).
             10 PI-DRKI3534-G48VFILIALENAME   PIC  X(0035).
             10 PI-DRKI3534-G48VFILIALEORT   PIC  X(0035).
             10 PI-DRKI3534-G48VFILIALESTR   PIC  X(0035).
             10 PI-DRKI3534-G48VGEBDAT   PIC  X(0008).
             10 PI-DRKI3534-G48VGESCHLECHT   PIC  X(0001).
             10 PI-DRKI3534-G48VHAUSNR   PIC  X(0009).
             10 PI-DRKI3534-G48VINFOSPERDAT   PIC  X(0008).
             10 PI-DRKI3534-G48VINFOSPRREKZ   PIC  X(0001).
             10 PI-DRKI3534-G48VKNDNAME   PIC  X(0028).
             10 PI-DRKI3534-G48VLKZ   PIC  X(0003).
             10 PI-DRKI3534-G48VMANDANT   PIC S9(9) COMP.
             10 PI-DRKI3534-G48VNACHNAME   PIC  X(0035).
             10 PI-DRKI3534-G48VONLBANKING   PIC  X(0001).
             10 PI-DRKI3534-G48VORT   PIC  X(0028).
             10 PI-DRKI3534-G48VPERSONLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3534-G48VPLZ   PIC  X(0009).
             10 PI-DRKI3534-G48VPOSTFACH   PIC  X(0009).
             10 PI-DRKI3534-G48VSTR   PIC  X(0028).
             10 PI-DRKI3534-G48VTELBANKING   PIC  X(0001).
             10 PI-DRKI3534-G48VTELVOR   PIC  X(0006).
             10 PI-DRKI3534-G48VTESLNR   PIC  X(0009).
             10 PI-DRKI3534-G48VVBNR   PIC S9(9) COMP.
             10 PI-DRKI3534-G48VVERFUEGUNG   PIC  X(0001).
             10 PI-DRKI3534-G48VVERTENDE   PIC  X(0008).
             10 PI-DRKI3534-G48VVERTGBEGINN   PIC  X(0008).
             10 PI-DRKI3534-G48VVORNAME   PIC  X(0035).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3534-NI1.
           05 PI-DRKI3534-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3534-NI7.
           05 PI-DRKI3534-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3534.
           05  PO-DRKI3534-RC                     PIC  X(003).
               88 PO-DRKI3534-RC-OK               VALUE 'OK '.
               88 PO-DRKI3534-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
