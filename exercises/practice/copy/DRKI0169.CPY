      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0169.CPY  $
      *    $Revision:   1.5  $
      *
      *    $Log:   L:/MF/CPY/DRKI0169.CPv  $
      *
      *   Rev 1.5   07 Nov 2001 17:03:42   t13ex12
      *AKI00250 - DRKI0169 geändert
      *
      *   Rev 1.3   Aug 02 1999 15:03:48   T13EX49
      *AKI00023 - Erweiterung für BFG
      *
      *   Rev 1.2   Jun 21 1999 13:43:28   T13EX49
      *AKI00023 - 004 - Deutschland
      *
      *   Rev 1.1   09 Feb 1999 11:02:12   T13EX12
      *AKI00014 - Umsetzung Laenderschl. fuer DIT, BFG
      *
      *   Rev 1.0   29 Sep 1997 11:54:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 1 6 9    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER Umsetzung Länder-Kennzeichen von DIT und     *
      *    BfG in die Länder-Kennz. der Supporttabelle.                *
      *                                                                *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *03.02.99!NEUERSTELLUNG                          !V.D.GOLTZ! 01  *
      *KI-NEU  !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *23.02.99!004 für BFG                            !PeterPohl! 02  *
      *KI-NEU  !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
990521*21.05.99!093 für HR gelöscht                    !P. Pohl  ! 03  *
990521*KI-NEU  !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
990623*23.06.99!801 für PNG eingefügt                  !P. Pohl  ! 04  *
990623*KI-NEU  !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
061101*06.11.01!Neue Codes von DIT eingefügt, z.b.     !v.d.Goltz! 05  *
      *        ! für Belgien,Luxemburg.                !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
      * Felder für das Umsetzen des Länderschlüssels
       01  Z-LAND-SCHL           PIC  X(003).
       01  Z-LAND-SCHL-VALUE.

           05 FILLER             PIC  X(006) VALUE '000D  '.
           05 FILLER             PIC  X(006) VALUE '001F  '.
           05 FILLER             PIC  X(006) VALUE '003NL '.
230299     05 FILLER             PIC  X(006) VALUE '004D  '.
           05 FILLER             PIC  X(006) VALUE '005I  '.
           05 FILLER             PIC  X(006) VALUE '006GB '.
           05 FILLER             PIC  X(006) VALUE '007IRL'.
           05 FILLER             PIC  X(006) VALUE '008DK '.
           05 FILLER             PIC  X(006) VALUE '009GR '.
           05 FILLER             PIC  X(006) VALUE '010P  '.
           05 FILLER             PIC  X(006) VALUE '011E  '.
061101     05 FILLER             PIC  X(006) VALUE '017B  '.
...        05 FILLER             PIC  X(006) VALUE '018L  '.
061101*    05 FILLER             PIC  X(006) VALUE '021E  '.
           05 FILLER             PIC  X(006) VALUE '024IS '.
061101*    05 FILLER             PIC  X(006) VALUE '025FR '.
           05 FILLER             PIC  X(006) VALUE '028N  '.
           05 FILLER             PIC  X(006) VALUE '030S  '.
           05 FILLER             PIC  X(006) VALUE '032FIN'.
061101*    05 FILLER             PIC  X(006) VALUE '036CH '.
           05 FILLER             PIC  X(006) VALUE '037FL '.
           05 FILLER             PIC  X(006) VALUE '038A  '.
           05 FILLER             PIC  X(006) VALUE '039CH '.
030299     05 FILLER             PIC  X(006) VALUE '041FR '.
           05 FILLER             PIC  X(006) VALUE '043AND'.
           05 FILLER             PIC  X(006) VALUE '044GBZ'.
           05 FILLER             PIC  X(006) VALUE '045V  '.
           05 FILLER             PIC  X(006) VALUE '046M  '.
061101     05 FILLER             PIC  X(006) VALUE '047RSM'.
061101*    05 FILLER             PIC  X(006) VALUE '048YU '.
           05 FILLER             PIC  X(006) VALUE '052TR '.
           05 FILLER             PIC  X(006) VALUE '053EST'.
           05 FILLER             PIC  X(006) VALUE '054LV '.
           05 FILLER             PIC  X(006) VALUE '055LT '.
           05 FILLER             PIC  X(006) VALUE '060PL '.
           05 FILLER             PIC  X(006) VALUE '061CZ '.
           05 FILLER             PIC  X(006) VALUE '063SK '.
           05 FILLER             PIC  X(006) VALUE '064H  '.
           05 FILLER             PIC  X(006) VALUE '066RO '.
           05 FILLER             PIC  X(006) VALUE '068BG '.
           05 FILLER             PIC  X(006) VALUE '070AL '.
           05 FILLER             PIC  X(006) VALUE '072UA '.
           05 FILLER             PIC  X(006) VALUE '073BY '.
           05 FILLER             PIC  X(006) VALUE '074MD '.
           05 FILLER             PIC  X(006) VALUE '075RUS'.
           05 FILLER             PIC  X(006) VALUE '076GE '.
           05 FILLER             PIC  X(006) VALUE '078AZ '.
           05 FILLER             PIC  X(006) VALUE '079KZ '.
           05 FILLER             PIC  X(006) VALUE '080TM '.
           05 FILLER             PIC  X(006) VALUE '082TJ '.
           05 FILLER             PIC  X(006) VALUE '083KS '.
           05 FILLER             PIC  X(006) VALUE '091SLO'.
           05 FILLER             PIC  X(006) VALUE '092HR '.
990521*    05 FILLER             PIC  X(006) VALUE '093HR '.
           05 FILLER             PIC  X(006) VALUE '093BIH'.
           05 FILLER             PIC  X(006) VALUE '094YU '.
           05 FILLER             PIC  X(006) VALUE '096MK '.
           05 FILLER             PIC  X(006) VALUE '102B  '.
           05 FILLER             PIC  X(006) VALUE '104L  '.
           05 FILLER             PIC  X(006) VALUE '191MC '.
           05 FILLER             PIC  X(006) VALUE '204MA '.
           05 FILLER             PIC  X(006) VALUE '208DZ '.
           05 FILLER             PIC  X(006) VALUE '212TN '.
061101     05 FILLER             PIC  X(006) VALUE '216LAR'.
           05 FILLER             PIC  X(006) VALUE '220ET '.
061101     05 FILLER             PIC  X(006) VALUE '224EAS'.
061101     05 FILLER             PIC  X(006) VALUE '228RIM'.
061101     05 FILLER             PIC  X(006) VALUE '232RMM'.
           05 FILLER             PIC  X(006) VALUE '236BF '.
061101     05 FILLER             PIC  X(006) VALUE '240RN '.
...        05 FILLER             PIC  X(006) VALUE '247KAV'.
...        05 FILLER             PIC  X(006) VALUE '248SN '.
...        05 FILLER             PIC  X(006) VALUE '252WAG'.
...        05 FILLER             PIC  X(006) VALUE '257GBI'.
...        05 FILLER             PIC  X(006) VALUE '260GUI'.
...        05 FILLER             PIC  X(006) VALUE '264WAL'.
061101     05 FILLER             PIC  X(006) VALUE '268LB '.
180199     05 FILLER             PIC  X(006) VALUE '272CI '.
180199     05 FILLER             PIC  X(006) VALUE '276GH '.
061101     05 FILLER             PIC  X(006) VALUE '280RT '.
180199     05 FILLER             PIC  X(006) VALUE '284DY '.
           05 FILLER             PIC  X(006) VALUE '288WAN'.
061101     05 FILLER             PIC  X(006) VALUE '302RFC'.
...        05 FILLER             PIC  X(006) VALUE '306RCA'.
...        05 FILLER             PIC  X(006) VALUE '310AG '.
...        05 FILLER             PIC  X(006) VALUE '314GAB'.
...        05 FILLER             PIC  X(006) VALUE '318RCB'.
...        05 FILLER             PIC  X(006) VALUE '322ZRE'.
...        05 FILLER             PIC  X(006) VALUE '324RWA'.
...        05 FILLER             PIC  X(006) VALUE '328RU '.
...        05 FILLER             PIC  X(006) VALUE '330ANG'.
...        05 FILLER             PIC  X(006) VALUE '334ETH'.
...        05 FILLER             PIC  X(006) VALUE '336ER '.
...        05 FILLER             PIC  X(006) VALUE '338DSI'.
...        05 FILLER             PIC  X(006) VALUE '342SP '.
...        05 FILLER             PIC  X(006) VALUE '346EAK'.
...        05 FILLER             PIC  X(006) VALUE '350EAU'.
...        05 FILLER             PIC  X(006) VALUE '352EAT'.
...        05 FILLER             PIC  X(006) VALUE '355SY '.
...        05 FILLER             PIC  X(006) VALUE '366MOC'.
...        05 FILLER             PIC  X(006) VALUE '370RM '.
...        05 FILLER             PIC  X(006) VALUE '372REU'.
...        05 FILLER             PIC  X(006) VALUE '373MS '.
061101     05 FILLER             PIC  X(006) VALUE '378Z  '.
180199     05 FILLER             PIC  X(006) VALUE '382SW '.
061101     05 FILLER             PIC  X(006) VALUE '386MW '.
           05 FILLER             PIC  X(006) VALUE '388ZA '.
180199     05 FILLER             PIC  X(006) VALUE '389NAM'.
           05 FILLER             PIC  X(006) VALUE '400USA'.
           05 FILLER             PIC  X(006) VALUE '404CDN'.
           05 FILLER             PIC  X(006) VALUE '412MEX'.
061101     05 FILLER             PIC  X(006) VALUE '413BER'.
...        05 FILLER             PIC  X(006) VALUE '416GCA'.
...        05 FILLER             PIC  X(006) VALUE '421BH '.
...        05 FILLER             PIC  X(006) VALUE '424HN '.
...        05 FILLER             PIC  X(006) VALUE '428ES '.
061101     05 FILLER             PIC  X(006) VALUE '432NIC'.
           05 FILLER             PIC  X(006) VALUE '436CR '.
061101     05 FILLER             PIC  X(006) VALUE '442PA '.
...        05 FILLER             PIC  X(006) VALUE '446ANL'.
061101     05 FILLER             PIC  X(006) VALUE '448C  '.
180199     05 FILLER             PIC  X(006) VALUE '452RH '.
           05 FILLER             PIC  X(006) VALUE '453BS '.
030299     05 FILLER             PIC  X(006) VALUE '456DOM'.
061101     05 FILLER             PIC  X(006) VALUE '459ANT'.
...        05 FILLER             PIC  X(006) VALUE '460WD '.
...        05 FILLER             PIC  X(006) VALUE '464JA '.
...        05 FILLER             PIC  X(006) VALUE '465WL '.
061101     05 FILLER             PIC  X(006) VALUE '467WV '.
           05 FILLER             PIC  X(006) VALUE '469BDS'.
061101     05 FILLER             PIC  X(006) VALUE '473WG '.
061101     05 FILLER             PIC  X(006) VALUE '478NA '.
           05 FILLER             PIC  X(006) VALUE '480CO '.
           05 FILLER             PIC  X(006) VALUE '484YV '.
061101     05 FILLER             PIC  X(006) VALUE '500EC '.
           05 FILLER             PIC  X(006) VALUE '504PE '.
           05 FILLER             PIC  X(006) VALUE '508BR '.
           05 FILLER             PIC  X(006) VALUE '512RCH'.
           05 FILLER             PIC  X(006) VALUE '516BOL'.
           05 FILLER             PIC  X(006) VALUE '520PY '.
           05 FILLER             PIC  X(006) VALUE '524ROU'.
           05 FILLER             PIC  X(006) VALUE '528RA '.
           05 FILLER             PIC  X(006) VALUE '600CY '.
           05 FILLER             PIC  X(006) VALUE '604RL '.
           05 FILLER             PIC  X(006) VALUE '608SYR'.
180199     05 FILLER             PIC  X(006) VALUE '616IR '.
           05 FILLER             PIC  X(006) VALUE '624IL '.
           05 FILLER             PIC  X(006) VALUE '628JOR'.
           05 FILLER             PIC  X(006) VALUE '632KSA'.
           05 FILLER             PIC  X(006) VALUE '636KWT'.
           05 FILLER             PIC  X(006) VALUE '640BRN'.
           05 FILLER             PIC  X(006) VALUE '644Q  '.
           05 FILLER             PIC  X(006) VALUE '647UAE'.
           05 FILLER             PIC  X(006) VALUE '649OM '.
061101     05 FILLER             PIC  X(006) VALUE '653ADN'.
           05 FILLER             PIC  X(006) VALUE '662PK '.
           05 FILLER             PIC  X(006) VALUE '664IND'.
061101     05 FILLER             PIC  X(006) VALUE '667MLD'.
061101     05 FILLER             PIC  X(006) VALUE '669CL '.
180199     05 FILLER             PIC  X(006) VALUE '680THA'.
061101     05 FILLER             PIC  X(006) VALUE '690VN '.
           05 FILLER             PIC  X(006) VALUE '700RI '.
091198     05 FILLER             PIC  X(006) VALUE '701MAL'.
           05 FILLER             PIC  X(006) VALUE '706SGP'.
061101     05 FILLER             PIC  X(006) VALUE '708RP '.
061101     05 FILLER             PIC  X(006) VALUE '720PRC'.
           05 FILLER             PIC  X(006) VALUE '728ROK'.
           05 FILLER             PIC  X(006) VALUE '732J  '.
           05 FILLER             PIC  X(006) VALUE '736RC '.
           05 FILLER             PIC  X(006) VALUE '740HK '.
           05 FILLER             PIC  X(006) VALUE '800AUS'.
990623     05 FILLER             PIC  X(006) VALUE '801PNG'.
           05 FILLER             PIC  X(006) VALUE '804NZ '.
061101     05 FILLER             PIC  X(006) VALUE '815FJI'.
061101     05 FILLER             PIC  X(006) VALUE '819WS '.

       01  Z-LAND-SCHL-ARR REDEFINES Z-LAND-SCHL-VALUE.

061101*    05 Z-LAND-SCHL-OCC                OCCURS 111.
061101     05 Z-LAND-SCHL-OCC                OCCURS 170.
              10  Z-LAND-SCHL-DIT   PIC  X(003).
              10  Z-LAND-SCHL-KI    PIC  X(003).
061101*01  Z-LAND-SCHL-MAX          PIC S9(009) VALUE 111.
061101 01  Z-LAND-SCHL-MAX          PIC S9(009) VALUE 170.

      * INDEX FüR DAS UMSETZEN DES LÄNDERSCHLUESSELS
       01  I-LAND-SCHL              PIC  9(004) VALUE 0.


