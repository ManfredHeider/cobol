      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.10.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1604.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1604.CPv  $
      *
      *   Rev 2.1   Jan 12 2004 10:32:20   t13ex27
      *AKI00384 - EA-Anforderungen und IOS
      *
      *   Rev 2.0   Oct 28 2003 17:48:02   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1604                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1604                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  29.10.2003/15:47:37
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
      *    FÜR DIE FUNKTION DRKI1604
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1604.
           05  PI-DRKI1604-KEY.
             10 PI-DRKI1604-EAPCNR           PIC S9(9) COMP.
             10 PI-DRKI1604-EADATUM          PIC X(10).

           05  PI-DRKI1604-DATEN.
             10 PI-DRKI1604-EADIREKTION      PIC S9(4) COMP.
             10 PI-DRKI1604-EAPCBESITZERID   PIC X(13).
             10 PI-DRKI1604-EAPCTYP          PIC X(1).
             10 PI-DRKI1604-EAPGM            PIC X(8).
             10 PI-DRKI1604-EATESTLAUF       PIC X(1).
      *
       01  PO-DRKI1604.
           05  PO-DRKI1604-RC                 PIC  X(003).
               88 PO-DRKI1604-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1604-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
