      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.08.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1941.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1941.CPv  $
      *
      *   Rev 2.1   Dec 20 2006 15:55:20   t13ex43
      *FKI00979: Fremdverträge mit mehreren VN
      *
      *   Rev 2.0   Aug 29 2006 15:11:50   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1941                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1941                    *
      *                                                               *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00556!29.08.06!2.1 ! neu                       !I. Frenzel*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1941
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRKI1941.
           05  PI-DRKI1941-KEY.
               10   PI-DRKI1941-VRBPERSONENID     PIC X(13).
               10   PI-DRKI1941-VRBPABADRESSEID   PIC X(13).
      *
       01  PO-DRKI1941.
           05  PO-DRKI1941-RC                 PIC  X(003).
               88 PO-DRKI1941-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1941-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1941-DATEN.
               10   PO-DRKI1941-COUNTER       PIC S9(4) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
