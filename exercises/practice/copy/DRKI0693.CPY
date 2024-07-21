      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0693.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0693.CPv  $
      *
      *   Rev 2.1   Apr 08 1999 16:24:14   T13EX51
      *AKI00021
      *
      *   Rev 2.0   Mar 01 1999 15:07:32   T13EX51
      *Neue Revision AKI000001
      *
      *   Rev 1.0   25 Aug 1998 09:31:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0693                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0693                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  20.10.98/08.22.47
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
      *    FÜR DIE FUNKTION DRKI0693
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0693.
           05  PI-DRKI0693-KEY.


             10 PI-DRKI0693-UMSGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0693-UMSLAUFENDENR   PIC S9(4) COMP.
             10 PI-DRKI0693-UMSVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0693.
           05  PO-DRKI0693-RC                 PIC  X(003).
               88 PO-DRKI0693-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0693-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
