      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1403.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1403.CPv  $
      *
      *   Rev 2.1   May 08 2002 09:50:14   t13ex49
      *AKI00274 - erstmalige Übergabe
      *
      *   Rev 2.0   Apr 09 2002 08:33:42   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1403                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1403                    *
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
      *    FšR DIE FUNKTION DRKI1403
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1403.
           05  PI-DRKI1403-KEY.


             10 PI-DRKI1403-RDTADB24ERSTELLT   PIC X(26).
             10 PI-DRKI1403-RDTADB24VBID   PIC X(13).

      *
       01  PO-DRKI1403.
           05  PO-DRKI1403-RC                 PIC  X(003).
               88 PO-DRKI1403-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1403-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1403-DATEN.


             10 PO-DRKI1403-RDTADB24ERL-DAT   PIC X(10).
             10 PO-DRKI1403-RDTADB24ERL-KZ   PIC X(01).
             10 PO-DRKI1403-RDTADB24HERKUNFT   PIC X(02).


      *
      ******* ENDE COPY-MEMBER **************************************
