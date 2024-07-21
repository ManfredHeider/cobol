      *---------------------------------------------------------------*
      *    Standard f³r alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1043.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1043.CPv  $
      *
      *   Rev 2.2   Sep 30 2009 14:50:16   t13ex51
      *AKI00852: KI-Host - Kundensuche
      *
      *   Rev 2.1   Mar 30 2000 11:06:24   T13EX39
      *AKI00098 OZPs
      *
      *   Rev 2.0   Feb 09 2000 14:17:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1043                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1043                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  13.03.00/16.38.10
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
      *    FÜR DIE FUNKTION DRKI1043
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1043.
           05  PI-DRKI1043-KEY.


             10 PI-DRKI1043-PERSONENID   PIC X(13).

      *
       01  PO-DRKI1043.
           05  PO-DRKI1043-RC                 PIC  X(003).
               88 PO-DRKI1043-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1043-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1043-DATEN.


             10 PO-DRKI1043-LETZTEAENDERUNG   PIC X(26).
             10 PO-DRKI1043-PERSONANREDEID   PIC S9(4) COMP.
             10 PO-DRKI1043-NI-PERSONANREDEID   PIC S9(4) COMP.
             10 PO-DRKI1043-PERSONANZNAME   PIC X(30).
             10 PO-DRKI1043-PERSONANZVORNAME   PIC X(15).
             10 PO-DRKI1043-PERSONEMPFOHLVON.
                15 PO-DRKI1043-PRSNMPFHLVN-LN   PIC S9(4) COMP.
                15 PO-DRKI1043-PRSNMPFHLVN-TXT   PIC X(50).

             10 PO-DRKI1043-PERSONGEBGRUENDDAT   PIC X(10).
             10 PO-DRKI1043-PERSONKUNDENART   PIC X(01).
             10 PO-DRKI1043-PERSONNAMEPHONCODE   PIC X(15).
             10 PO-DRKI1043-PERSONNOTIZID   PIC X(13).
             10 PO-DRKI1043-PERSONSTEUERBERAT.
                15 PO-DRKI1043-PRSNSTRBRT-LN   PIC S9(4) COMP.
                15 PO-DRKI1043-PRSNSTRBRT-TXT   PIC X(50).

             10 PO-DRKI1043-PERSONSTEUERBTELEF.
                15 PO-DRKI1043-PRSNSTRBTLF-LN   PIC S9(4) COMP.
                15 PO-DRKI1043-PRSNSTRBTLF-TXT   PIC X(20).

             10 PO-DRKI1043-PERSONSUCHNAME   PIC X(15).
             10 PO-DRKI1043-PERSONSUCHVORNAME   PIC X(15).


      *
      ******* ENDE COPY-MEMBER **************************************
