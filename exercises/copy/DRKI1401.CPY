      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1401.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1401.CPv  $
      *
      *   Rev 2.1   May 08 2002 09:50:10   t13ex49
      *AKI00274 - erstmalige Übergabe
      *
      *   Rev 2.0   Apr 09 2002 08:33:32   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1401                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1401                    *
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
      *    FšR DIE FUNKTION DRKI1401
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1401.
           05  PI-DRKI1401-KEY.


             10 PI-DRKI1401-RDTADB24ERSTELLT   PIC X(26).
             10 PI-DRKI1401-RDTADB24VBID       PIC X(13).


           05  PI-DRKI1401-DATEN.


             10 PI-DRKI1401-RDTADB24ERL-KZ   PIC X(01).
             10 PI-DRKI1401-RDTADB24HERKUNFT   PIC X(02).

      *
       01  PO-DRKI1401.
           05  PO-DRKI1401-RC                 PIC  X(003).
               88 PO-DRKI1401-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1401-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
