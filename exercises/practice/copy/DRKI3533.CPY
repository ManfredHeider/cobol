      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.12.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3533.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3533.CPv  $
      *
      *   Rev 2.4   Nov 24 2020 08:31:44   u130685
      *AKI01805: G48_Vertrag OZP/Copy
      *
      *   Rev 2.3   Feb 19 2019 06:46:14   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.2   Feb 18 2019 16:30:12   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.1   Jan 30 2019 11:19:46   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.0   Dec 6 2018 15:46:08 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3533                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3533                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 14:04:35 / U130685
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
      *    FÜR DIE FUNKTION DRKI3533
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3533.
           05  PI-DRKI3533-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3533-G48VVERTRAGSNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI3533.
           05  PO-DRKI3533-RC                     PIC  X(003).
               88 PO-DRKI3533-RC-OK               VALUE 'OK '.
               88 PO-DRKI3533-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3533-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3533-G48VANACHLASSKZ   PIC  X(0001).
             10 PO-DRKI3533-G48VANREDE   PIC  X(0001).
             10 PO-DRKI3533-G48VBIC   PIC  X(0011).
             10 PO-DRKI3533-G48VBLZ   PIC  X(0008).
             10 PO-DRKI3533-G48VDELKZ   PIC  X(0001).
             10 PO-DRKI3533-G48VDEPOTKZ   PIC  X(0001).
             10 PO-DRKI3533-G48VESAFE   PIC  X(0001).
             10 PO-DRKI3533-G48VFILIALEART   PIC  X(0035).
             10 PO-DRKI3533-G48VFILIALENAME   PIC  X(0035).
             10 PO-DRKI3533-G48VFILIALEORT   PIC  X(0035).
             10 PO-DRKI3533-G48VFILIALESTR   PIC  X(0035).
             10 PO-DRKI3533-G48VGEBDAT   PIC  X(0008).
             10 PO-DRKI3533-G48VGESCHLECHT   PIC  X(0001).
             10 PO-DRKI3533-G48VHAUSNR   PIC  X(0009).
             10 PO-DRKI3533-G48VINFOSPERDAT   PIC  X(0008).
             10 PO-DRKI3533-G48VINFOSPRREKZ   PIC  X(0001).
             10 PO-DRKI3533-G48VKNDNAME   PIC  X(0028).
             10 PO-DRKI3533-G48VLKZ   PIC  X(0003).
             10 PO-DRKI3533-G48VMANDANT   PIC S9(9) COMP.
             10 PO-DRKI3533-G48VNACHNAME   PIC  X(0035).
             10 PO-DRKI3533-G48VONLBANKING   PIC  X(0001).
             10 PO-DRKI3533-G48VORT   PIC  X(0028).
             10 PO-DRKI3533-G48VPERSONLFDNR   PIC S9(4) COMP.
             10 PO-DRKI3533-G48VPLZ   PIC  X(0009).
             10 PO-DRKI3533-G48VPOSTFACH   PIC  X(0009).
             10 PO-DRKI3533-G48VSTR   PIC  X(0028).
             10 PO-DRKI3533-G48VTELBANKING   PIC  X(0001).
             10 PO-DRKI3533-G48VTELVOR   PIC  X(0006).
             10 PO-DRKI3533-G48VTESLNR   PIC  X(0009).
             10 PO-DRKI3533-G48VVBNR   PIC S9(9) COMP.
             10 PO-DRKI3533-G48VVERFUEGUNG   PIC  X(0001).
             10 PO-DRKI3533-G48VVERTENDE   PIC  X(0008).
             10 PO-DRKI3533-G48VVERTGBEGINN   PIC  X(0008).
             10 PO-DRKI3533-G48VVORNAME   PIC  X(0035).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
