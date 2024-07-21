      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2096.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2096.CPv  $
      *
      *   Rev 2.1   03 Dec 2007 11:14:54   t13ex27
      *AKL00664: Allfinanz: Status VNR erledigt
      *
      *   Rev 2.0   17 Sep 2007 11:24:58   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2096                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2096                    *
      *                                                               *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKL00664!13.09.07!2.1 ! neu                       !I. Frenzel*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2096
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRKI2096.
           05  PI-DRKI2096-KEY.
               10   PI-DRKI2096-VRBPERSONENID        PIC  X(013).
               10   PI-DRKI2096-VRBPERSONENROLLEKZ   PIC  X(001).
               10   PI-DRKI2096-VRBVERTRAGSIDVON     PIC  X(013).
               10   PI-DRKI2096-VRBVERTRAGSIDBIS     PIC  X(013).
      *
       01  PO-DRKI2096.
           05  PO-DRKI2096-RC                 PIC  X(003).
               88 PO-DRKI2096-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2096-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2096-DATEN.
               10   PO-DRKI2096-COUNTER       PIC S9(4) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
