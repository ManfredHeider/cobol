      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2522.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2522.CPv  $
      *
      *   Rev 2.2   Sep 06 2012 14:28:30   t13ae49
      *AKI00943: Std.-OZP für Tabelle KRANKENPERSEIGEN
      *
      *   Rev 2.1   Sep 06 2012 10:58:40   t13ex14
      *AKI00943: Std.-OZP für Tabelle KRANKENPERSEIGEN
      *
      *   Rev 2.0   Nov 3 2011 18:47:35 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2522                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2522                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         03.11.2011 / 18:47:35 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2522
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2522.
           05  PI-DRKI2522-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2522-KPEPERSONLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2522-KPEVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2522-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2522-KPEBEGINNDATUM   PIC  X(010).
             10 PI-DRKI2522-KPEENDEDATUM   PIC  X(010).
END   *


      *
       01  PO-DRKI2522.
           05  PO-DRKI2522-RC                     PIC  X(003).
               88 PO-DRKI2522-RC-OK               VALUE 'OK '.
               88 PO-DRKI2522-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
