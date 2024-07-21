      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0027.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0027.CPv  $
      *
      *   Rev 2.2   Apr 18 2007 20:24:56   t13ae34
      *AAN00655: monatl. Batchläufe - Teil. 2
      *
      *   Rev 2.1   11 Sep 2000 12:26:12   T13AE34
      *AAN00279-konzessionspflichtiges Geschäft
      *
      *   Rev 2.0   Feb 27 1999 11:05:34   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:35:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0027  FUNKTION OZP VSAM                  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    08.09.2000                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00279!08.09.00! 2.1! NEUERSTELLUNG             ! HEIDER   *
      * AAN00655!18.04.07! 2.2! 3-ST. PS                  ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0027                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<        LESEN PRODUKTIONSERGEBNISDATEI     *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0027.
          10 PI-DRAN0027-UFKT                  PIC  X.
              88 PI-DRAN0027-UFKT-READ-FIRST   VALUE '1'.
              88 PI-DRAN0027-UFKT-READ-NEXT    VALUE '2'.
              88 PI-DRAN0027-UFKT-READ-LAST    VALUE '3'.
           10  PI-DRAN0027-PKEY.
               20  PI-DRAN0027-PMANR           PIC 9(7)      COMP-3.
               20  PI-DRAN0027-PSTATDAT        PIC 9(7)      COMP-3.
180407*        20  PI-DRAN0027-PSTATSCHL       PIC 9(5)      COMP-3.
180407         20  PI-DRAN0027-PSTATSCHL.
280307             30 PI-DRAN0027-ERG-KZ          PIC 9(001).
280307             30 PI-DRAN0027-PGEB-GES        PIC 9(003) COMP-3.
280307             30 PI-DRAN0027-PROD-PROD-SCHL  PIC 9(003) COMP-3.
      *
       01 PO-DRAN0027.
          10 PO-DRAN0027-RC                    PIC  X(003).
             88 PO-DRAN0027-RC-OK              VALUE 'OK '.
             88 PO-DRAN0027-RC-NOT-FOUND       VALUE 'NFD'.

          10 PO-DRAN0027-PSATZA.
             20 PO-DRAN0027-PEIGEN.
               30 PO-DRAN0027-PEAEINH          PIC S9(8)V9   COMP-3.
               30 PO-DRAN0027-PESTCK           PIC S9(7)     COMP-3.
               30 PO-DRAN0027-PESUM            PIC S9(11)    COMP-3.
               30 PO-DRAN0027-PEBEITR          PIC S9(11)V99 COMP-3.
             20 PO-DRAN0027-PGRUPPE.
               30 PO-DRAN0027-PGRAEINH         PIC S9(8)V9   COMP-3.
               30 PO-DRAN0027-PGRSTCK          PIC S9(7)     COMP-3.
               30 PO-DRAN0027-PGRSUM           PIC S9(11)    COMP-3.
               30 PO-DRAN0027-PGRBEITR         PIC S9(11)V99 COMP-3.

