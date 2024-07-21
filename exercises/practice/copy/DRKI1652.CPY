      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.03.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1652.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1652.CPv  $
      *
      *   Rev 2.2   Sep 18 2012 15:14:00   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-KVB
      *
      *   Rev 2.1   Apr 07 2004 14:21:50   t13ex27
      *Persönliche Anrede für mehrfachbetreute Kunden
      *
      *   Rev 2.0   Mar 04 2004 13:21:46   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1652                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1652                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         15.11.2011 / 15:27:25 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI1652
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1652.
           05  PI-DRKI1652-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI1652-KVBKIPERSONENID   PIC  X(0013).
             10 PI-DRKI1652-KVBVBPERSONENID   PIC  X(0013).
             10 PI-DRKI1652-KVBVBPERSONENROLLE   PIC  X(0001).
END   *


           05  PI-DRKI1652-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI1652-KVBLOESCHKZ   PIC  X(0001).
END   *


      *
       01  PO-DRKI1652.
           05  PO-DRKI1652-RC                     PIC  X(003).
               88 PO-DRKI1652-RC-OK               VALUE 'OK '.
               88 PO-DRKI1652-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
