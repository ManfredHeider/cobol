      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.01.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3057.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3057.CPv  $
      *
      *   Rev 2.1   Mar 14 2016 09:56:46   t13ex27
      *AKI01400: Redesign UPKI0203(Kundenübertragung)-UP für Änderu
      *ng EVM-KZ und AVM
      *
      *   Rev 2.0   Jan 21 2016 11:11:26   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3057                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3057                    *
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
       01  PI-DRKI3057.

             10 PI-DRKI3057-VERTRAGSID       PIC  X(0013).
             10 PI-DRKI3057-LFDNR            PIC S9(4) COMP.
             10 PI-DRKI3057-PRODUKTID        PIC S9(4) COMP.

             10 PI-DRKI3057-EIGENVERMITTELT  PIC  X(0001).
      *
       01  PO-DRKI3057.
           05  PO-DRKI3057-RC                     PIC  X(003).
               88 PO-DRKI3057-RC-OK               VALUE 'OK '.
               88 PO-DRKI3057-RC-NOT-FOUND        VALUE 'NFD'.
               88 PO-DRKI3057-RC-NOK              VALUE 'NOK'.
      *
      *
      ******* ENDE COPY-MEMBER ***************************************

