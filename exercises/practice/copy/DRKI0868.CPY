      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0868.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0868.CPv  $
      *
      *   Rev 2.2   Sep 18 2012 15:13:26   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-KVB
      *
      *   Rev 2.0   Nov 15 2011 15:27:23 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0868                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0868                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         15.11.2011 / 15:27:23 / T13EX27
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0868
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0868.
           05  PI-DRKI0868-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI0868-KVBKIPERSONENID   PIC  X(0013).
             10 PI-DRKI0868-KVBVBPERSONENID   PIC  X(0013).
             10 PI-DRKI0868-KVBVBPERSONENROLLE   PIC  X(0001).
END   *

      *
       01  PO-DRKI0868.
           05  PO-DRKI0868-RC                     PIC  X(003).
               88 PO-DRKI0868-RC-OK               VALUE 'OK '.
               88 PO-DRKI0868-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
