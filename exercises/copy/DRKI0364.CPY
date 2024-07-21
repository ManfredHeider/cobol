      * T13AE38   Stand: 06/10/98    08:26:09
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0364.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0364.CPv  $
      *
      *   Rev 2.0   Mar 23 1999 15:52:40   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:02:26   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*


      *  Update Provision

      *****************************************************************
      *    COPY-MEMBER: DRKI0364                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0364                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  05.10.98/18.11.37
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
      *    FÜR DIE FUNKTION DRKI0364
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0364.
           05  PI-DRKI0364-KEY.


             10 PI-DRKI0364-LTTTTNR   PIC S9(4) COMP.
             10 PI-DRKI0364-LTTVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0364-LTTVERTRAGSID   PIC X(13).


           05  PI-DRKI0364-DATEN.


             10 PI-DRKI0364-LTTABSCHLUSSVERM   PIC S9(9) COMP.
             10 PI-DRKI0364-LTTPROVHAFTZBIS   PIC X(10).
             10 PI-DRKI0364-LTTPROVHAFTZVON   PIC X(10).


      *
       01  PO-DRKI0364.
           05  PO-DRKI0364-RC                 PIC  X(003).
               88 PO-DRKI0364-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0364-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
