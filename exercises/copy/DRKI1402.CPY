      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1402.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1402.CPv  $
      *
      *   Rev 2.1   May 08 2002 09:50:12   t13ex49
      *AKI00274 - erstmalige Übergabe
      *
      *   Rev 2.0   Apr 09 2002 08:33:36   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1402                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1402                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  22.04.2002/12:59:57
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI1402
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1402.
           05  PI-DRKI1402-KEY.


             10 PI-DRKI1402-RDTADB24ERSTELLT   PIC X(26).
             10 PI-DRKI1402-RDTADB24VBID   PIC X(13).


           05  PI-DRKI1402-DATEN.


             10 PI-DRKI1402-RDTADB24ERL-DAT   PIC X(10).
             10 PI-DRKI1402-RDTADB24ERL-KZ   PIC X(01).

      *
       01  PO-DRKI1402.
           05  PO-DRKI1402-RC                 PIC  X(003).
               88 PO-DRKI1402-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1402-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
