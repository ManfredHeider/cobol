      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.04.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2592.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2592.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 14:48:22   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP BANKV
      *BDGEIGEN
      *
      *   Rev 2.0   Apr 13 2012 11:54:38 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2592                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2592                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.04.2012 / 11:54:38 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2592
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2592.
           05  PI-DRKI2592-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2592-BVVVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2592.
           05  PO-DRKI2592-RC                     PIC  X(003).
               88 PO-DRKI2592-RC-OK               VALUE 'OK '.
               88 PO-DRKI2592-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2592-DATEN.
             10   PO-DRKI2592-COUNTER           PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
