      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.06.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2409.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2409.CPv  $
      *
      *   Rev 2.1   Aug 10 2010 11:51:16   t13ex39
      *AKI00966: DVAG Österreich: Anpassung der VB-Nummernprüfung U
      *PKI0209
      *
      *   Rev 2.0   Jun 18 2010 10:00:41 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2409                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2409                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         18.06.2010 / 15:25:09 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2409
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  COUNT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2409.
           05  PI-DRKI2409-KEY.
ANF   *        %M_PI_KEY%
             10 PI-DRKI2409-MITARBEITERNR   PIC S9(9) COMP.
             10 PI-DRKI2409-MITARBMANDANTNR   PIC S9(9) COMP.
END   *
      *
       01  PO-DRKI2409.
           05  PO-DRKI2409-RC                     PIC  X(003).
               88 PO-DRKI2409-RC-OK               VALUE 'OK '.
               88 PO-DRKI2409-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2409-DATEN.
             10   PO-DRKI2409-COUNTER           PIC S9(4) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
