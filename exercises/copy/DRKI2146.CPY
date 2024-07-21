      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2146.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2146.CPv  $
      *
      *   Rev 2.1   17 Oct 2007 12:06:30   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   28 Sep 2007 14:49:30   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2146                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2146                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  17.10.2007/11:22:33
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
      *    FÜR DIE FUNKTION DRKI2146
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2146.
           05  PI-DRKI2146-KEY.


             10 PI-DRKI2146-VRARISIKOHOSTID   PIC S9(9) COMP.
             10 PI-DRKI2146-VRAVERTRAGSID   PIC X(13).
             

           05  PI-DRKI2146-DATEN.


             10 PI-DRKI2146-VRARISIKOLKZ   PIC S9(9) COMP.
             10 PI-DRKI2146-VRARISIKOORT   PIC X(40).
             10 PI-DRKI2146-VRARISIKOPLZ   PIC X(6).
             10 PI-DRKI2146-VRARISIKOSTRASSE   PIC X(40).
             

      *
       01  PO-DRKI2146.
           05  PO-DRKI2146-RC                 PIC  X(003).
               88 PO-DRKI2146-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2146-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
