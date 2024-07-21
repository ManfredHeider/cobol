      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1386.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1386.CPv  $
      *
      *   Rev 2.1   Jul 30 2002 13:39:36   t13ex39
      *AKI00274 DB/24
      *
      *   Rev 2.0   Apr 09 2002 08:32:10   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1386                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1386                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  17.04.2002/12:30:36
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
      *    FšR DIE FUNKTION DRKI1386
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1386.
           05  PI-DRKI1386-KEY.


             10 PI-DRKI1386-EVP-ID   PIC X(12).
             10 PI-DRKI1386-EVP-ORDER-NUMBER   PIC S9(9) COMP.


           05  PI-DRKI1386-DATEN.


             10 PI-DRKI1386-EVP-CC-ID   PIC S9(9) COMP.
             10 PI-DRKI1386-EVP-STRING-DATA.
                15 PI-DRKI1386-VP-STRNG-DT-LN   PIC S9(4) COMP.
                15 PI-DRKI1386-VP-STRNG-DT-TXT   PIC X(200).

             10 PI-DRKI1386-EVP-SUBTYPE   PIC S9(4) COMP.


      *
       01  PO-DRKI1386.
           05  PO-DRKI1386-RC                 PIC  X(003).
               88 PO-DRKI1386-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1386-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
