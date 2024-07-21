      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.03.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2236.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2236.CPv  $
      *
      *   Rev 2.1   Mar 28 2008 16:20:54   t13ex51
      *AKI00705: Anpassung Kundenaktualisierung am Host
      *
      *   Rev 2.0   Mar 19 2008 16:57:58   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2236                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2236                    *
      *                                                               *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00705!19.03.08!1   ! neu                       !HRO       *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2236
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRKI2236.
           05  PI-DRKI2236-KEY.
               10   PI-DRKI2236-VRBPERSONENID     PIC X(13).
      *
       01  PO-DRKI2236.
           05  PO-DRKI2236-RC                 PIC  X(003).
               88 PO-DRKI2236-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2236-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2236-DATEN.
               10   PO-DRKI2236-COUNTER       PIC S9(4) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************

