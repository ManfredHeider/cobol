      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3097.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3097.CPv  $
      *
      *   Rev 2.2   Dec 19 2016 13:09:22   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.1   Dec 07 2016 10:50:34   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.0   Sep 21 2016 11:52:05 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3097                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3097                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.12.2016 / 12:52:43 / T13AC20
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01466!21.09.16!2.1 ! NEUERSTELLUNG             ! SWO      *
      * AKI01466!19.12.16!2.2 ! FELDER ENTFERNT:          ! SWO      *
      *         !        !    !   - G51VRAHMENVERT        !          *
      *         !        !    !   - G51VABRECHDAT         !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3097
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3097.
           05  PI-DRKI3097-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3097-G51VVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3097.
           05  PO-DRKI3097-RC                     PIC  X(003).
               88 PO-DRKI3097-RC-OK               VALUE 'OK '.
               88 PO-DRKI3097-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3097-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3097-G51VABRECHKZ   PIC  X(0001).
             10 PO-DRKI3097-G51VABSCHLVBNR   PIC S9(9) COMP.
             10 PO-DRKI3097-G51VANREDE   PIC S9(4) COMP.
             10 PO-DRKI3097-G51VANTRAGSPIN   PIC S9(9) COMP.
             10 PO-DRKI3097-G51VGEBDAT   PIC  X(0008).
             10 PO-DRKI3097-G51VGESCHLECHT   PIC  X(0001).
             10 PO-DRKI3097-G51VLANDID   PIC  X(0003).
             10 PO-DRKI3097-G51VMAKLERDAT   PIC  X(0008).
             10 PO-DRKI3097-G51VMANDANT   PIC S9(9) COMP.
             10 PO-DRKI3097-G51VNACHNAME   PIC  X(0030).
             10 PO-DRKI3097-G51VORT   PIC  X(0030).
             10 PO-DRKI3097-G51VPLZ   PIC  X(0009).
             10 PO-DRKI3097-G51VSTATUS   PIC  X(0001).
             10 PO-DRKI3097-G51VSTRASSE   PIC  X(0040).
             10 PO-DRKI3097-G51VVERSART   PIC  X(0001).
             10 PO-DRKI3097-G51VVORNAME   PIC  X(0030).
             10 PO-DRKI3097-G51VVORVENDDATUM   PIC  X(0008).
             10 PO-DRKI3097-G51VVORVGES   PIC S9(4) COMP.
             10 PO-DRKI3097-G51VVORVKUENDKZ   PIC  X(0001).
             10 PO-DRKI3097-G51VVRTRGBEGINN   PIC  X(0008).
             10 PO-DRKI3097-G51VVRTRGENDE   PIC  X(0008).
             10 PO-DRKI3097-G51VZUSRAHMEN   PIC  X(0001).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

