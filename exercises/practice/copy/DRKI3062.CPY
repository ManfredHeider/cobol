      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.03.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3062.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3062.CPv  $
      *
      *   Rev 2.1   Mar 14 2016 09:57:00   t13ex27
      *AKI01400: Redesign UPKI0203(Kundenübertragung)-UP für Änderu
      *ng EVM-KZ und AVM
      *
      *   Rev 2.0   Mar 01 2016 10:53:48   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3062                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3062                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01400!21.01.16!2.1 ! NEU                       ! IFR      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3015
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRKI3062.

             10 PI-DRKI3062-VERTRAGSID       PIC  X(0013).
             10 PI-DRKI3062-LFDNR            PIC S9(4) COMP.
             10 PI-DRKI3062-PRODUKTID        PIC S9(4) COMP.

             10 PI-DRKI3062-ABSCHLUSSVBNR    PIC S9(9) USAGE COMP.
      *
       01  PO-DRKI3062.
           05  PO-DRKI3062-RC                     PIC  X(003).
               88 PO-DRKI3062-RC-OK               VALUE 'OK '.
               88 PO-DRKI3062-RC-NOT-FOUND        VALUE 'NFD'.
               88 PO-DRKI3062-RC-NOK              VALUE 'NOK'.
      *
      *
      ******* ENDE COPY-MEMBER ***************************************

