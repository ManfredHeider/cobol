      * T13AE38   Stand: 06/10/98    08:26:17
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0365.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0365.CPv  $
      *
      *   Rev 2.0   Mar 23 1999 15:52:42   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:02:30   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*


      *  Delete

      *****************************************************************
      *    COPY-MEMBER: DRKI0365                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0365                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  05.10.98/18.11.36
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
      *    FÜR DIE FUNKTION DRKI0365
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0365.
           05  PI-DRKI0365-KEY.


             10 PI-DRKI0365-LTTTTNR   PIC S9(4) COMP.
             10 PI-DRKI0365-LTTVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0365-LTTVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0365.
           05  PO-DRKI0365-RC                 PIC  X(003).
               88 PO-DRKI0365-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0365-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
