      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0480.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0480.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:57:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 13:30:40   I13PC48
      *Funktionen Tab. 347
      *
      *   Rev 1.0   28 Jul 1998 09:57:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0480.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:/MF/CPY/DRKI0480.CPv  $
      *
      *   Rev 1.0   28 Jul 1998 09:57:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0480                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0480                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  19.08.98/10.01.25
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
      *    FÜR DIE FUNKTION DRKI0480
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0480.
           05  PI-DRKI0480-KEY.


             10 PI-DRKI0480-VEGGEFAHRCODE   PIC S9(4) COMP.
             10 PI-DRKI0480-VEGVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0480-VEGVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0480.
           05  PO-DRKI0480-RC                 PIC  X(003).
               88 PO-DRKI0480-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0480-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
